package pe.gob.servir.entidad.repository.impl;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Repository;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import feign.FeignException;
import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiBuscarCorreo;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.CorreoPersonaDTO;
import pe.gob.servir.entidad.model.DatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.model.DetalleUoDTO;
import pe.gob.servir.entidad.model.EmpleadoDTO;
import pe.gob.servir.entidad.model.EvaluadorEvaluadoDTO;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.ParticipanteDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadoresServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadosServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteServidorCivilDTO;
import pe.gob.servir.entidad.model.PersonasPuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.PuestoDTO;
import pe.gob.servir.entidad.model.PuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO;
import pe.gob.servir.entidad.model.RespBuscarParticipantesEntidadServidorCivilDTO;
import pe.gob.servir.entidad.model.RespParticipanteDTO;
import pe.gob.servir.entidad.model.ServidorCivilDTO;
import pe.gob.servir.entidad.repository.DetUnidadOrganicaRepository;
import pe.gob.servir.entidad.repository.ServidoresCivilRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilGDRDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespApiPersonaCorreo;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Repository
public class ServidoresCivilesRepositoryImpl implements ServidoresCivilRepository {

	private static final Logger LOGGER = LoggerFactory.getLogger(ServidoresCivilesRepositoryImpl.class);

	@PersistenceContext(unitName = "entidadEntityManagerFactory", type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@Autowired
	BeanAdapterServidorCivil beanAdapter;

	@Autowired
	PersonaApiClient personaApiClient;

	@Autowired
	DetUnidadOrganicaRepository detUnidadOrganicaRepository;

	@Autowired
	SeguridadApiClient seguridadApiClient;

	@Value("${auth.appl.clientid.clientsecret.encode.base64}")
	private String tokenApplication;
	
	@Value("${jboss.private.base.url.seguridad}")
	private String uriBaseSeguridad;
	
	@Value("${jboss.private.base.url.persona}")
	private String uriBasePersona;
	
	private final ObjectMapper objectMapper = new ObjectMapper();
	
	@Override
	public RespBase<GenericResponseMessage> insertarServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request, MyJsonWebToken jwt)
			throws Exception {
		return insertarServidorCivil(request, jwt, false);
	}

	@SuppressWarnings({ "rawtypes", "unchecked"})
	@Override
	public RespBase<GenericResponseMessage> insertarServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request, MyJsonWebToken jwt, boolean esParaMasivo)
			throws Exception {

		RespBase<GenericResponseMessage> responseServidores = new RespBase<>();
		RespBase<ApiPersonaRequestDTO> requestPersona = null;
		RespBase<ApiBuscarCorreo> requestCorreo = null;
		RespBase<RespApiPersona> responsePersona = null;
		RespBase<RespApiPersonaCorreo> responseCorreo = null;
		String token = null;
		
		List<GenericResponseMessage> responseGeneric = null;
		
		GenericResponseMessage genericResponseMessage = new GenericResponseMessage();

			ServidorCivilGDRDTO bean = request.getPayload().getServidorCivil();
			requestPersona = beanAdapter.adapToBeanPersona(bean);
			LOGGER.info("[REQUEST PERSONA: ] " + JsonUtil.convertirObjetoACadenaJson(requestPersona));
			
			try {		
				
				List<ServidorCivilDTO> servidorCivilDTO = new ArrayList<>();
				requestCorreo = beanAdapter.adapToBeanCorreo(bean);
				LOGGER.info("[REQUEST CORREO: ] " + JsonUtil.convertirObjetoACadenaJson(requestCorreo));
				
			//	responseCorreo = personaApiClient.buscarCorreo(requestCorreo);
				servidorCivilDTO = buscarCorreosExistentesServidoresCiviles(bean.getCorreoElectronico());
				
				if(servidorCivilDTO.size()>0) {
					ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE,
							"Correo Existe");
					return responseServidores;
				}
				/*if (Boolean.FALSE.equals(responseCorreo.getStatus().getSuccess())) {
					ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE,
							"El servicio de la API Persona no responde. Por favor verifique que el servicio este funcionando correctamente.");
					return responseServidores;
				}
				
				if (Boolean.TRUE.equals(responseCorreo.getStatus().getSuccess())) {
					
					LOGGER.info("[REQUEST CORREO: ] " + JsonUtil.convertirObjetoACadenaJson(responseCorreo));
					
					LOGGER.info("[REQUEST CORREO: ] " + JsonUtil.convertirObjetoACadenaJson(responseCorreo));
					LOGGER.info("[REQUEST CORREO: ] " + JsonUtil.convertirObjetoACadenaJson(responseCorreo));
					
					if(responseCorreo.getPayload() != null) {
						ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE,
								"Correo Existe");
						return responseServidores;
					}
				} */
				
				if (esParaMasivo) {
					token = this.obtenerToken();
					if (token == null) {
						LOGGER.error("Error al obtener token para registrarPersonaNatural");
						ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE,
								"Error al obtener token. Por favor verifique que el servicio este funcionando correctamente.");
						return responseServidores;
					}

					RestTemplate restTemplate = new RestTemplate();
				    HttpHeaders headers = new HttpHeaders();
				    headers.setContentType(MediaType.APPLICATION_JSON);
			   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

	   		   	    HttpEntity<Object> rqPersona = new HttpEntity<>(requestPersona, headers);
			   	    
	   		   	    ResponseEntity<String> respPersona = restTemplate.postForEntity(this.uriBasePersona.concat(Constantes.ENDPOINT_INS_PERSONA_NATURAL), rqPersona, String.class);
   					responsePersona = objectMapper.readValue(respPersona.getBody(), new TypeReference<RespBase<RespApiPersona>>() {});
				} else {
					responsePersona = personaApiClient.registrarPersonaNatural(requestPersona);
				}
				
				if (Boolean.FALSE.equals(responsePersona.getStatus().getSuccess())) {
					ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE,
							"El servicio de la API Persona no responde. Por favor verifique que el servicio este funcionando correctamente.");
					return responseServidores;
				}

				LOGGER.info("[RESPONSE PERSONA: ] " + JsonUtil.convertirObjetoACadenaJson(responsePersona));

			} catch (FeignException e) {
				String error = "";
				
				try {
					error = e.getMessage().substring(e.getMessage().indexOf("{"), e.getMessage().length() - 1);
				} catch (Exception ex) {
					return ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE, null,
							"Ocurrio un error al invocar el servicio de Api Persona");
				}				
				
				RespBase<Object> respo = JsonUtil.convertirCadenaJsonPostAObjeto(error, RespBase.class);

				if (e.getMessage().contains(Constantes.SERVER_500)) {
					String msjeError = "";
					if (String.join(" ", respo.getStatus().getError().getMessages())
							.contains("java.net.UnknownHostException: wsanscreniec.servir.gob.pe")) {
						msjeError = "La validación con el servicio RENIEC no esta respondiendo. Por favor verifique que el servicio este funcionando correctamente";
					}					
					ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE, msjeError);
					return responseServidores;
				} else {
					if(String.join(" ", respo.getStatus().getError().getMessages()).trim().equalsIgnoreCase("documento ya existe")
							|| respo.getStatus().getError().getCode().trim().equalsIgnoreCase("CORREO")) {
						responsePersona = personaApiClient.obtienePersonaPorDocumento(bean.getTipoDocumento(), bean.getNumeroDocumento());
						LOGGER.info("[RESPONSE PERSONA: ] " + JsonUtil.convertirObjetoACadenaJson(responsePersona));
						if (responsePersona.getStatus().getSuccess().equals(Boolean.TRUE) && 
								(responsePersona.getPayload().getPersonaNatural().getPersonaId() == null)) {
							ParametrosUtil.setearListResponse(responseServidores, Boolean.FALSE, null,
									respo.getStatus().getError().getMessages());
							return responseServidores;
						}
					}else {						
						ParametrosUtil.setearListResponse(responseServidores, Boolean.FALSE, null,
								respo.getStatus().getError().getMessages());
						return responseServidores;
					}
				}
			} catch (HttpClientErrorException e) {
				String error = "";
				
				try {
					error = e.getMessage().substring(e.getMessage().indexOf("{"), e.getMessage().length() - 1);
				} catch (Exception ex) {
					return ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE, null,
							"Ocurrio un error al invocar el servicio de Api Persona");
				}				
				
				RespBase<Object> respo = JsonUtil.convertirCadenaJsonPostAObjeto(error, RespBase.class);

				if (e.getMessage().contains(Constantes.SERVER_500)) {
					String msjeError = "";
					if (String.join(" ", respo.getStatus().getError().getMessages())
							.contains("java.net.UnknownHostException: wsanscreniec.servir.gob.pe")) {
						msjeError = "La validación con el servicio RENIEC no esta respondiendo. Por favor verifique que el servicio este funcionando correctamente";
					}					
					ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE, msjeError);
					return responseServidores;
				} else {
					if(String.join(" ", respo.getStatus().getError().getMessages()).trim().equalsIgnoreCase("documento ya existe")
							|| respo.getStatus().getError().getCode().trim().equalsIgnoreCase("CORREO")) {
						
						if (esParaMasivo) {
							RestTemplate restTemplate = new RestTemplate();
						    HttpHeaders headers = new HttpHeaders();
						    headers.setContentType(MediaType.APPLICATION_JSON);
					   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

			   		   	    HttpEntity<Object> rqPersona = new HttpEntity<>(null, headers);
					   	    
			   		   	    Map<String, String> uriVariables = new HashMap<>();
			   		   	    uriVariables.put("tipoDocumento", String.valueOf(bean.getTipoDocumento()));
			   		   	    uriVariables.put("numeroDocumento", bean.getNumeroDocumento());
			   		 	
			   		   	    ResponseEntity<String> respPersona = restTemplate.exchange(this.uriBasePersona.concat(Constantes.ENDPOINT_OBTENER_PERSONA_BY_DOCUMENTO).concat("?tipoDocumento={tipoDocumento}").concat("&numeroDocumento={numeroDocumento}"), HttpMethod.GET, rqPersona, String.class, uriVariables);
		   					responsePersona = objectMapper.readValue(respPersona.getBody(), new TypeReference<RespBase<RespApiPersona>>() {});
						} else {
							responsePersona = personaApiClient.obtienePersonaPorDocumento(bean.getTipoDocumento(), bean.getNumeroDocumento());
						}
						
						LOGGER.info("[RESPONSE PERSONA: ] " + JsonUtil.convertirObjetoACadenaJson(responsePersona));
						if (responsePersona.getStatus().getSuccess().equals(Boolean.TRUE) && 
								(responsePersona.getPayload().getPersonaNatural().getPersonaId() == null)) {
							ParametrosUtil.setearListResponse(responseServidores, Boolean.FALSE, null,
									respo.getStatus().getError().getMessages());
							return responseServidores;
						}
					}else {						
						ParametrosUtil.setearListResponse(responseServidores, Boolean.FALSE, null,
								respo.getStatus().getError().getMessages());
						return responseServidores;
					}
				}
				
			}
			
			PuestoDTO requestPuesto = null;
			requestPuesto = beanAdapter.adapToBeanPuesto(request.getPayload().getEntidadId(), bean, jwt);

			EmpleadoDTO requestEmpleado = null;
			requestEmpleado = beanAdapter.adapToBeanEmpelado(bean, jwt, request, requestPuesto.getPuestoId(),
					responsePersona);

			DetalleUoDTO requestDetalleUo = null;
			requestDetalleUo = beanAdapter.adapToBeanDetalleUo(bean, jwt, requestPuesto, request, responsePersona);
            ///System.out.println("DETALLEEEEEEEEEEEEEEEEEEEEEEEE UOOO"+ requestDetalleUo.getDetalleUoId());
			///System.out.println("RESPONSABLEEEEEEEEEEEEEEEEEEEEEEEE"+ requestDetalleUo.getResponsable());
			responseGeneric = altaDeServidoresCiviles(responsePersona, requestPuesto, requestEmpleado,
					requestDetalleUo, bean);
			LOGGER.info("[RESPONSE STORED PROCEDURE: ] " + JsonUtil.convertirObjetoACadenaJson(responseGeneric));
						
			genericResponseMessage.setCodigo(responseGeneric.get(0).getCodigo());
			genericResponseMessage.setMensaje(responseGeneric.get(0).getMensaje());
			responseServidores.setPayload(genericResponseMessage);
			
			if (genericResponseMessage.getCodigo() == 0) {
				ParametrosUtil.setearResponse(responseServidores, Boolean.FALSE, null,
						responseGeneric.get(0).getMensaje());
			}else {
				ParametrosUtil.setearResponse(responseServidores, Boolean.TRUE, null,
						responseGeneric.get(0).getMensaje());
			}
			
			
			return responseServidores;		
	}

	private String obtenerToken() {
		RestTemplate restTemplate = new RestTemplate();
	    HttpHeaders headers = new HttpHeaders();
	    JsonNode root;
	    String token = null;
	    
	    try {
		    headers.setContentType(MediaType.APPLICATION_JSON);
//		   	headers.set(HttpHeaders.AUTHORIZATION, "Basic OTgyOThkNjktYzlmNS00OTg3LWEzOTUtZmU4ZWQ5ZWM5NzNhOjdjZjI4MTMzLTI0YjctNGVlOS1iOWE0LTk4OGQxNThlMGFkNg==");
		    headers.set(HttpHeaders.AUTHORIZATION, "Basic " + this.tokenApplication);
		    String cadenaAuthJson = "{\"trace\":{\"traceId\":\"string\"},\"payload\":{\"grantType\":\"clientCredentials\"}}";
		    HttpEntity<String> requestAuth = new HttpEntity<String>(cadenaAuthJson, headers);
	
		    LOGGER.info("Obteniendo token para usar los servicios web");
	   	    ResponseEntity<String> responseAuthStr = restTemplate.postForEntity(this.uriBaseSeguridad.concat(Constantes.ENDPOINT_GET_TOKEN), requestAuth, String.class);
	   	    
	   	    if (responseAuthStr.getStatusCode().compareTo(HttpStatus.OK) != 0) {
				LOGGER.error("Error al obtener token. Json={}", responseAuthStr.getBody());
				return null;
	   	    }

			root = objectMapper.readTree(responseAuthStr.getBody());
			token = root.at("/payload/accessToken").asText();
	   	    LOGGER.info("Token obtenido: {}", token);

		} catch (Exception e) {
			LOGGER.error("Error al obtener token. Error={}", e.getMessage());
			return null;
		}

   	    return token;
	}
	
	@SuppressWarnings("unchecked")
	private List<GenericResponseMessage> altaDeServidoresCiviles(RespBase<RespApiPersona> responsePersona,
			PuestoDTO requestPuesto, EmpleadoDTO requestEmpleado, DetalleUoDTO requestDetalleUo,
			ServidorCivilGDRDTO bean) {
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_ALTA_SERVIDORES_CIVILES, GenericResponseMessage.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, LocalDate.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(8, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(9, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(10, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(11, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(12, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(13, LocalDateTime.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(14, Integer.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(15, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(16, void.class, ParameterMode.REF_CURSOR);
		
		storedProcedure.setParameter(1, requestPuesto.getEntidadId());
		storedProcedure.setParameter(2, requestDetalleUo.getOrganigramaId());
		storedProcedure.setParameter(3, responsePersona.getPayload().getPersonaNatural().getPersonaId());
		storedProcedure.setParameter(4, requestEmpleado.getRegimenLaboral());
		storedProcedure.setParameter(5, StringUtils.trimToEmpty(requestEmpleado.getSindicatoFlag()));
		storedProcedure.setParameter(6, requestDetalleUo.getPuestoFechaInicio());
		storedProcedure.setParameter(7, StringUtils.trimToEmpty(bean.getCorreoElectronico()));
		storedProcedure.setParameter(8, StringUtils.trimToEmpty(requestDetalleUo.getResponsable()));
		storedProcedure.setParameter(9, StringUtils.trimToEmpty(requestDetalleUo.getExcluye()));
		storedProcedure.setParameter(10, StringUtils.trimToEmpty(requestPuesto.getDecripcion()));
		storedProcedure.setParameter(11, StringUtils.trimToEmpty(requestPuesto.getEstadoRegistro()));
		storedProcedure.setParameter(12, StringUtils.trimToEmpty(requestPuesto.getUsuarioCreacion()));
		storedProcedure.setParameter(13, requestPuesto.getFechaCreacion());
		storedProcedure.setParameter(14, requestDetalleUo.getTipoAsignacion());
		storedProcedure.setParameter(15, requestPuesto.getPuestoId());
		
		storedProcedure.execute();
		
		return storedProcedure.getResultList();
	}

	@SuppressWarnings("unchecked")
	private List<ServidorCivilDTO> buscarCorreosExistentesServidoresCiviles(String correo) { 
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_BUSCAR_CORREO_EXISTENTE_SERVIDOR_CIVIL, ServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		
		storedProcedure.setParameter(1, correo);
		storedProcedure.execute();
		
		return storedProcedure.getResultList();
	}
 
	@SuppressWarnings("unchecked")
	@Override
	public List<DatosPersonalesServidorCivilDTO> obtenerDatosPersonalesServidorCivil(Map<String, Object> parametroMap)
			throws ValidationException {
		List<DatosPersonalesServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_OBTENER_DATOS_PERSONALES_SERVIDOR_CIVIL,
				DatosPersonalesServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("detuoId"));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.setParameter(4, parametroMap.get("regimenId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<PuestoUoServidorCivilDTO> obtenerPuestoUoServidorCivil(Map<String, Object> parametroMap)
			throws ValidationException {
		List<PuestoUoServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_OBTENER_PUESTO_UO_SERVIDOR_CIVIL,
				PuestoUoServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<PersonasPuestoUoServidorCivilDTO> buscarPersonasPuestoUoServidorCivil(Map<String, Object> parametroMap)
			throws ValidationException {
		List<PersonasPuestoUoServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_BUSCAR_PERSONAS_PUESTO_UO_SERVIDOR_CIVIL,
				PersonasPuestoUoServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.setParameter(3, parametroMap.get("puestoId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteServidorCivilDTO> buscarParticipanteServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteServidorCivilDTO> lista;
		String estadoId = null;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_BUSCAR_PARTICIPANTES_SERVIDOR_CIVIL, 
				ParticipanteServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(8, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(9, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(10, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(11, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("tipoOrganoId"));
		storedProcedure.setParameter(3, parametroMap.get("unidadOrganicaSuperiorId"));
		storedProcedure.setParameter(4, parametroMap.get("unidadOrganicaId"));
		storedProcedure.setParameter(5, parametroMap.get("regimenLaboralId"));
		storedProcedure.setParameter(6, parametroMap.get("tipoDocumentoId"));
		storedProcedure.setParameter(7, parametroMap.get("datosServCivil"));
		storedProcedure.setParameter(8, parametroMap.get("numeroDocumento"));		
		estadoId = (parametroMap.get("estadoId") != null) ? String.valueOf(parametroMap.get("estadoId")) : null;
		storedProcedure.setParameter(9, estadoId);
		storedProcedure.setParameter(10, parametroMap.get("estadoSerCivGdrId"));
		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteServidorCivilDTO> buscarParticipanteServidorCivilNoActivos(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteServidorCivilDTO> lista;
		String estadoId = null;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_BUSCAR_PARTICIPANTES_SERVIDOR_CIVIL_NOACTIVOS, 
				ParticipanteServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(8, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(9, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(10, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(11, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("tipoOrganoId"));
		storedProcedure.setParameter(3, parametroMap.get("unidadOrganicaSuperiorId"));
		storedProcedure.setParameter(4, parametroMap.get("unidadOrganicaId"));
		storedProcedure.setParameter(5, parametroMap.get("regimenLaboralId"));
		storedProcedure.setParameter(6, parametroMap.get("tipoDocumentoId"));
		storedProcedure.setParameter(7, parametroMap.get("datosServCivil"));
		storedProcedure.setParameter(8, parametroMap.get("numeroDocumento"));		
		estadoId = (parametroMap.get("estadoId") != null) ? String.valueOf(parametroMap.get("estadoId")) : null;
		storedProcedure.setParameter(9, estadoId);
		storedProcedure.setParameter(10, parametroMap.get("estadoSerCivGdrId"));
		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadoresServidorCivilDTO> buscarParticipantesEvaluadoresServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadoresServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADORES_SERVIDOR_CIVIL, 
				ParticipanteEvaluadoresServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADOS_SERVIDOR_CIVIL, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.PERSONAEVALUADORID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosNoMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADOS_NO_MANDO_MEDIO_SERVIDOR_CIVIL, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.PERSONAEVALUADORID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADOS_SIN_EVALUADOR_MANDO_MEDIO_SERVIDOR_CIVIL, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("detUoId"));
		storedProcedure.setParameter(3, parametroMap.get("uoId"));
		storedProcedure.setParameter(4, parametroMap.get(Constantes.PERSONAEVALUADORID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_MANDO_MEDIO, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.PERSONAEVALUADORID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarEvaluadosSinEvaluadoresServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADOS_SIN_EVALUADORES, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public EvaluadorEvaluadoDTO buscarEvaluadorEvaluado(Map<String, Object> parametroMap) throws ValidationException {
		EvaluadorEvaluadoDTO result = null;
		
		try{
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_DATOS_EVALUADOR_Y_EVALUADO, 
				EvaluadorEvaluadoDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("detUOId"));
		storedProcedure.execute();
		result = (EvaluadorEvaluadoDTO) storedProcedure.getSingleResult();
		}
		catch (NoResultException nre){
			
			}
		return result;
	}
	
	@Override
	public ParticipanteDTO buscarParticipante(Map<String, Object> parametroMap) throws ValidationException {
		ParticipanteDTO result = null;
		
		try{
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_DATOS_PARTICIPANTE, 
				ParticipanteDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("detUOId"));
		storedProcedure.setParameter(2, parametroMap.get("segmentoId"));
		storedProcedure.execute();
		result = (ParticipanteDTO) storedProcedure.getSingleResult();
		}
		catch (NoResultException nre){
			
			}
		return result;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosPersonaServidorCivil(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PERSONAS_SERVIDOR_CIVIL, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<CorreoPersonaDTO> buscarCorreoPersona(Long personaId) {
		List<CorreoPersonaDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_CORREO_PERSONA, 
				CorreoPersonaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, personaId);
		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}	
	
	@SuppressWarnings("unchecked")
	@Override
	public List<CorreoPersonaDTO> buscarCorreoPersonaGestor(Long entidadId, Long tipoGestorid) {
		List<CorreoPersonaDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_CORREO_PERSONA_GESTOR, 
				CorreoPersonaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.setParameter(2, tipoGestorid);
		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadoresServidorCivilEntidad(Map<String, Object> parametroMap) throws ValidationException {
		List<ParticipanteEvaluadosServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADOS_SIN_EVALUADORES_ENTIDAD, 
				ParticipanteEvaluadosServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<RespBuscarParticipantesEntidadServidorCivilDTO> buscarParticipantesServidorCivilEntidad(
			Map<String, Object> parametroMap) throws ValidationException {
		List<RespBuscarParticipantesEntidadServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADORES_Y_SIN_EVALUADORES_ENTIDAD, 
				RespBuscarParticipantesEntidadServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("segmentoId"));
		storedProcedure.setParameter(3, parametroMap.get("uoId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO> buscarParticipantesEvaluadosServidorCivilEntidad(Map<String, Object> parametroMap) throws ValidationException {
		List<RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_BUSCAR_PARTICIPANTES_EVALUADOS_SERVIDOR_CIVIL_ENTIDAD, 
				RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("uoId"));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.PERSONAEVALUADORID));
		storedProcedure.setParameter(4, parametroMap.get("segmentoId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<RespParticipanteDTO> buscarParticipanteByDUOId(Map<String, Object> parametroMap) throws ValidationException {
		List<RespParticipanteDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA + "." + Constantes.SP_OBTENER_DATOS_PARTICIPANTE, RespParticipanteDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class, ParameterMode.REF_CURSOR);

		storedProcedure.setParameter(1, parametroMap.get("detUnidadOrganicaId"));
		storedProcedure.setParameter(2, parametroMap.get("personaId"));
		storedProcedure.setParameter(3, parametroMap.get("segmentoId"));
		storedProcedure.setParameter(4, parametroMap.get("rolId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

}