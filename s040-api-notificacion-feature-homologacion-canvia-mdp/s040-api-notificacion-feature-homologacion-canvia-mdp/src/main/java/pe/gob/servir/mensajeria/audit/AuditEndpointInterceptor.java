package pe.gob.servir.mensajeria.audit;

import java.lang.reflect.Method;
import java.time.Instant;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.InstantSerializer;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.mensajeria.common.RoutingKey;
import pe.gob.servir.mensajeria.queue.producer.RabbitProducer;
import pe.gob.servir.mensajeria.request.ReqBase;
import pe.gob.servir.mensajeria.security.MyJsonWebToken;

@Component
public class AuditEndpointInterceptor extends HandlerInterceptorAdapter {

	private static final Logger logger = LoggerFactory.getLogger(AuditEndpointInterceptor.class);
	private static final String ANONIMO = "ANONIMO";

	@Autowired
	private RabbitProducer rabbitProducer;
	@Value("${artifact.id}")
	private String moduleName;

	private ObjectMapper mapper = new ObjectMapper();

	@Override
	public boolean preHandle(HttpServletRequest servletRequest, HttpServletResponse servletResponse, Object handler)
			throws Exception {
		try {
			if (handler instanceof HandlerMethod) {
				String loggedUser = ANONIMO;
				String loggedApplicationName = ANONIMO;
				MyJsonWebToken jwt = (MyJsonWebToken) servletRequest.getAttribute("jwt");
				if (jwt != null) {
					loggedUser = jwt.getUsuario().getUsuario();
					loggedApplicationName = jwt.getAplicacion().getNombre();
				}
				HandlerMethod handlerMethod = (HandlerMethod) handler;
				Method endpointMethod = handlerMethod.getMethod();
				if (endpointMethod.isAnnotationPresent(AuditableEndpoint.class)) {
					AuditableEndpoint annotation = endpointMethod.getAnnotation(AuditableEndpoint.class);
					String httpMethod = servletRequest.getMethod();
					String path = servletRequest.getRequestURI().replace(servletRequest.getContextPath(), "");
					String jsonParameters = null;
					if (HttpMethod.GET.matches(httpMethod)) {
						Map<String, String[]> queryParameters = servletRequest.getParameterMap();
						jsonParameters = mapper.writeValueAsString(queryParameters);
					}

					EndpointLog endpointLog = new EndpointLog();
					endpointLog.setModuleName(moduleName);
					endpointLog.setEndpointName(annotation.endpointName());
					endpointLog.setMethodName(endpointMethod.getName());
					endpointLog.setHttpMethod(httpMethod);
					endpointLog.setPath(path);
					endpointLog.setParameters(jsonParameters);
					endpointLog.setLoggedUser(loggedUser);
					endpointLog.setLoggedApplicationName(loggedApplicationName);
					endpointLog.setOperationDate(Instant.now());

					ReqBase<EndpointLog> request = new ReqBase<>();
					request.setPayload(endpointLog);
					String jsonMessage = mapper.writeValueAsString(request);
					// Se escribe en la cola
					rabbitProducer.writeMessage(RoutingKey.LOG_SERVICE, jsonMessage);
				}
			}

		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
		return true;
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
	public static class EndpointLog {

		private String moduleName;
		private String endpointName;
		private String methodName;
		private String httpMethod;
		private String path;
		private String parameters;
		private String loggedUser;
		private String loggedApplicationName;
		@JsonSerialize(using = InstantSerializer.class)
		@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ssZ", timezone = "UTC")
		private Instant operationDate;
	}
}
