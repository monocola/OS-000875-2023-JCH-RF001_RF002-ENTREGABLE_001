package pe.gob.servir.mensajeria.audit;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;
import javax.servlet.http.HttpServletRequest;
import javax.transaction.Status;
import javax.transaction.Synchronization;

import org.hibernate.EmptyInterceptor;
import org.hibernate.Transaction;
import org.hibernate.type.Type;
import org.reflections.Reflections;
import org.reflections.scanners.FieldAnnotationsScanner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.jsr310.ser.InstantSerializer;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.mensajeria.common.RoutingKey;
import pe.gob.servir.mensajeria.queue.producer.RabbitProducer;
import pe.gob.servir.mensajeria.request.ReqBase;
import pe.gob.servir.mensajeria.security.MyJsonWebToken;

@Component
@SuppressWarnings("serial")
public class AuditEntityInterceptor extends EmptyInterceptor {

	private static final Logger logger = LoggerFactory.getLogger(AuditEntityInterceptor.class);
	private static final String ANONIMO = "ANONIMO";
	private static final String INSERT = "I";
	private static final String DELETE = "D";
	private static final String UPDATE = "U";

	private ObjectMapper mapper = new ObjectMapper();
	private ArrayList<TableLog> tableLogList = new ArrayList<>();
	private static RabbitProducer rabbitProducer;
	private static HttpServletRequest servletRequest;

	@Autowired
	public void setRabbitProducer(RabbitProducer rabbitProducer) {
		AuditEntityInterceptor.rabbitProducer = rabbitProducer; // NOSONAR
	}

	@Autowired
	public void setServletRequest(HttpServletRequest servletRequest) {
		AuditEntityInterceptor.servletRequest = servletRequest; // NOSONAR
	}

	@Override
	public boolean onSave(Object entity, Serializable id, Object[] state, String[] propertyNames, Type[] types) {
		if (entity instanceof AuditableEntity) {
			addItem(INSERT, entity, id, state, null, propertyNames, types);
		}
		return super.onSave(entity, id, state, propertyNames, types);
	}

	@Override
	public void onDelete(Object entity, Serializable id, Object[] state, String[] propertyNames, Type[] types) {
		if (entity instanceof AuditableEntity) {
			addItem(DELETE, entity, id, null, state, propertyNames, types);
		}
		super.onDelete(entity, id, state, propertyNames, types);
	}

	@Override
	public boolean onFlushDirty(Object entity, Serializable id, Object[] currentState, Object[] previousState,
			String[] propertyNames, Type[] types) {
		if (entity instanceof AuditableEntity) {
			addItem(UPDATE, entity, id, currentState, previousState, propertyNames, types);
		}
		return super.onFlushDirty(entity, id, currentState, previousState, propertyNames, types);
	}

	@Override
	public void afterTransactionBegin(Transaction tx) {
		tx.registerSynchronization(new SynchronizationCallback());
	}

	private void addItem(String operation, Object entity, Serializable id, Object[] currentState,
			Object[] previousState, String[] propertyNames, Type[] types) {
		try {
			String loggedUser = ANONIMO;
			String loggedApplicationName = ANONIMO;
			MyJsonWebToken jwt = (MyJsonWebToken) servletRequest.getAttribute("jwt");
			if (jwt != null) {
				loggedUser = jwt.getUsuario().getUsuario();
				loggedApplicationName = jwt.getAplicacion().getNombre();
			}
			String schemaName = entity.getClass().getAnnotation(Table.class).schema();
			String tableName = entity.getClass().getAnnotation(Table.class).name();
			String idJson = buildJson(entity, id);
			String previewStateJson = previousState == null ? null : buildJson(previousState, propertyNames, types);
			String currentStateJson = currentState == null ? null : buildJson(currentState, propertyNames, types);
			TableLog tableLog = new TableLog();
			tableLog.setSchemaName(schemaName);
			tableLog.setTableName(tableName);
			tableLog.setId(idJson);
			tableLog.setPreviewState(previewStateJson);
			tableLog.setCurrentState(currentStateJson);
			tableLog.setOperation(operation);
			tableLog.setLoggedUser(loggedUser);
			tableLog.setLoggedApplicationName(loggedApplicationName);
			tableLog.setOperationDate(Instant.now());
			tableLogList.add(tableLog);

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}

	private String buildJson(Object entity, Serializable id) throws JsonProcessingException {
		if (entity.getClass().isAnnotationPresent(IdClass.class)) {
			return mapper.writeValueAsString(id);
		}
		List<Field> fields = new Reflections(entity.getClass().getPackage().getName(), new FieldAnnotationsScanner())
				.getFieldsAnnotatedWith(Id.class).stream()
				.filter(field -> field.getDeclaringClass().equals(entity.getClass())).collect(Collectors.toList());
		if (fields.size() == 1) {
			ObjectNode objectNode = mapper.createObjectNode();
			objectNode.putPOJO(fields.get(0).getName(), id);
			return mapper.writeValueAsString(objectNode);
		}
		return "{}";
	}

	private String buildJson(Object[] state, String[] propertyNames, Type[] types) throws JsonProcessingException {
		ObjectNode objectNode = mapper.createObjectNode();
		for (int i = 0; i < state.length; i++) {
			if (!propertyNames[i].equalsIgnoreCase("_identifierMapper")) {
				if (state[i] == null) {
					objectNode.putNull(propertyNames[i]);
				} else if (isJavaTime(types[i])) {
					objectNode.put(propertyNames[i], state[i].toString());
				} else {
					objectNode.putPOJO(propertyNames[i], state[i]);
				}
			}
		}
		return mapper.writeValueAsString(objectNode);
	}

	private boolean isJavaTime(Type type) {
		switch (type.getName()) {
		case "Instant":
		case "LocalDate":
			return true;
		default:
			return false;
		}
	}

	/**
	 * Subclase auditoria
	 * 
	 * @author ttorres
	 *
	 */
	@Getter
	@Setter
	@ToString
	public static class TableLog implements Serializable {

		private String schemaName;
		private String tableName;
		private String id;
		private String previewState;
		private String currentState;
		private String transactionUuid;
		private String operation;
		private String loggedUser;
		private String loggedApplicationName;
		@JsonSerialize(using = InstantSerializer.class)
		@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ssZ", timezone = "UTC")
		private Instant operationDate;
	}

	private class SynchronizationCallback implements Synchronization {

		@Override
		public void beforeCompletion() {
			//
		}

		@Override
		public void afterCompletion(int status) {
			if (Status.STATUS_COMMITTED == status && !tableLogList.isEmpty()) {
				logger.info("{}", tableLogList);
				if (AuditEntityInterceptor.rabbitProducer != null) {
					String transactionUuid = UUID.randomUUID().toString();
					ReqBase<TableLog> request = null;
					String jsonMessage = null;
					for (TableLog tableLog : tableLogList) {
						try {
							tableLog.setTransactionUuid(transactionUuid);
							request = new ReqBase<>();
							request.setPayload(tableLog);
							jsonMessage = mapper.writeValueAsString(request);
							// Se escribe en la cola
							AuditEntityInterceptor.rabbitProducer.writeMessage(RoutingKey.LOG_TABLE, jsonMessage);

						} catch (JsonProcessingException e) {
							logger.error(e.getMessage(), e);
						}
					}
				}
			}
			tableLogList = new ArrayList<>();
		}

	}
}
