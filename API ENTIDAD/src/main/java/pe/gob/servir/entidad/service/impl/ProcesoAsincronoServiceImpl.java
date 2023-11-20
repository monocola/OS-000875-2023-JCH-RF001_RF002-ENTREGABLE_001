package pe.gob.servir.entidad.service.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import javax.xml.bind.DatatypeConverter;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.util.Strings;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiFileServerDTO;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.DataEmail;
import pe.gob.servir.entidad.api.dto.ReqEmail;
import pe.gob.servir.entidad.api.dto.ReqEmailAttachment;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.RoutingKey;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.Organigrama;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.Proceso;
import pe.gob.servir.entidad.model.ProcesoError;
import pe.gob.servir.entidad.model.Puesto;
import pe.gob.servir.entidad.queue.producer.RabbitProducer;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.OrganigramaRepository;
import pe.gob.servir.entidad.repository.ProcesoErrorRepository;
import pe.gob.servir.entidad.repository.ProcesoRepository;
import pe.gob.servir.entidad.repository.PuestoRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqProcesoAsincrono;
import pe.gob.servir.entidad.request.ReqRegistrarProcesoAsincrono;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilExcelDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilGDRDTO;
import pe.gob.servir.entidad.response.RespApiFile;
import pe.gob.servir.entidad.response.RespApiSeguridadUsuario;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtieneLista;
import pe.gob.servir.entidad.response.RespObtieneLista3;
import pe.gob.servir.entidad.response.RespRegistrarProcesoAsincrono;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.ProcesoAsincronoService;
import pe.gob.servir.entidad.service.ServidorCivilService;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.FilesUtil;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class ProcesoAsincronoServiceImpl implements ProcesoAsincronoService {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProcesoAsincronoServiceImpl.class);

	@Autowired
	private ProcesoRepository procesoRepository;

	@Autowired
	private ProcesoErrorRepository procesoErrorRepository;

	@Autowired
	private MaestraApiClient maestraApiClient;

	@Autowired
	private BeanAdapterServidorCivil adapterServidorCivil;

	@Autowired
	GeneralRepository generalRepository;

	@Autowired
	OrganigramaRepository organigramaRepository;
	
	@Autowired
	PuestoRepository puestoRepository;

	@Autowired
	ServidorCivilService servidorCivilService;

	@Autowired
	private RabbitProducer rabbitProducer;
	
	@Autowired
	VariablesSistema variablesSistema;
	
	@Value("${jboss.private.base.url.seguridad}")
	private String uriBaseSeguridad;
	
	@Value("${jboss.private.base.url.maestra}")
	private String uriBaseMaestra;
	
	@Value("${jboss.private.base.url.persona}")
	private String uriBasePersona;
	
	@Value("${jboss.private.base.url.notificacion}")
	private String uriBaseNotificacion;
	
	@Value("${auth.appl.clientid.clientsecret.encode.base64}")
	private String tokenApplication;

	private final ObjectMapper objectMapper = new ObjectMapper();
	
	@Override
	public RespBase<RespRegistrarProcesoAsincrono> registrarProcesoAsincrono(MyJsonWebToken jwt,
			ReqBase<ReqRegistrarProcesoAsincrono> request) throws Exception {
		
		LOGGER.info("Ejecutando servicio registrarProcesoAsincrono...");

		RespBase<RespRegistrarProcesoAsincrono> response = new RespBase<>();
		RespRegistrarProcesoAsincrono responsePayload = new RespRegistrarProcesoAsincrono();

		ReqRegistrarProcesoAsincrono requestDto = request.getPayload();

		RespBase<RespApiFile> subirFileInput = null;

		String rutaFile = null;

		if (Strings.isNotEmpty(requestDto.getBase64File())) {
			subirFileInput = this.maestraApiClient.insertImagen(this.setearDatosArchivo(requestDto));
			rutaFile = Boolean.TRUE.equals(subirFileInput.getStatus().getSuccess())
					&& Strings.isNotEmpty(subirFileInput.getPayload().getPathRelative())
							? subirFileInput.getPayload().getPathRelative()
							: "";
		}

		Proceso procesoAsincrono = this.setearProcesoAsincrono(requestDto, rutaFile, jwt);

		this.procesoRepository.save(procesoAsincrono);
		
		ReqProcesoAsincrono messageRabbit = new ReqProcesoAsincrono();
		messageRabbit.setProcesoId(procesoAsincrono.getProcesoId());
		messageRabbit.setTipoProcesoId(procesoAsincrono.getTipoProcesoId());
		
		String jsonMessage = new ObjectMapper().writeValueAsString(messageRabbit);
		rabbitProducer.writeMessage(RoutingKey.SEND_PROCESO_MASIVO, jsonMessage);
		
		responsePayload.setProcesoId(procesoAsincrono.getProcesoId());
		responsePayload.setCodigoRespuesta(0L);
		responsePayload.setMensajeRespuesta("Se envio el proceso masivo a la cola de procesos en forma exitosa");
		response.setPayload(responsePayload);
		response.getStatus().setSuccess(Boolean.TRUE);

		return response;
	}

	private ReqBase<ApiFileServerDTO> setearDatosArchivo(ReqRegistrarProcesoAsincrono requestDto) throws Exception {
		ReqBase<ApiFileServerDTO> request = new ReqBase<>();
		ApiFileServerDTO requestPayload = new ApiFileServerDTO();
		requestPayload.setFileBase64(requestDto.getBase64File());
		requestPayload.setExtension(requestDto.getExtensionArchivo());	// ".xlsm"
		requestPayload.setFileName(FilesUtil.generarNombreArchivoSinExt(requestDto.getNombreArchivo()));
		String pathFile = this.obtenerRutaFileAlfresco(requestDto);
		requestPayload.setPath(pathFile);
		requestPayload.setRatioDeCambio(Constantes.RATIO_CAMBIO_GDR);
		requestPayload.setResize(false);
		request.setPayload(requestPayload);
		return request;
	}

	@SuppressWarnings("unchecked")
	private String obtenerRutaFileAlfresco(ReqRegistrarProcesoAsincrono requestDto) throws Exception {
		String ruta = "";
		RespBase<RespObtieneLista> paramRutaAlfresco = this.maestraApiClient.obtieneParametros(Constantes.CODIGO_ALFRESCO);

		if (Boolean.TRUE.equals(paramRutaAlfresco.getStatus().getSuccess())) {
			for (RespObtieneLista.Parametro param : paramRutaAlfresco.getPayload().getItems()) {
				if (requestDto.getTipoProcesoId().equals(Constantes.TIPO_PROCESO_ASIN_CARGA_MASIVA_SERV_CIVIL) && 
						param.getCodigoTexto().equals(Constantes.RUTA_GME_CARGA_MAS_SERV_CIVIL)) {
					ruta = param.getValorTexto();
					break;
				}
			}
		}
		if (Strings.isNotEmpty(ruta)) {
			if (requestDto.getTipoProcesoId().equals(Constantes.TIPO_PROCESO_ASIN_CARGA_MASIVA_SERV_CIVIL)) {
				Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(requestDto.getJsonVariablesInput(), Map.class);
				String entidadId = parametros.get(Constantes.ENTIDADID).toString();
				
				ruta = ParametrosUtil.datePathReplaceRepositoryAlfresco(ruta);
				ruta = ruta.replace("{entidadId}", entidadId);
			}
		}
		
		return ruta;
	}

	private Proceso setearProcesoAsincrono(ReqRegistrarProcesoAsincrono requestDto, String pathRelative, MyJsonWebToken jwt) {
		Proceso procesoAsincrono = new Proceso();
		procesoAsincrono.setTipoProcesoId(requestDto.getTipoProcesoId());
		procesoAsincrono.setUsuarioEnvio(requestDto.getUsuarioEnvio());
		procesoAsincrono.setFechaEnvio(Instant.parse(requestDto.getFechaEnvio()));
		procesoAsincrono.setFechaFin(null);
		procesoAsincrono.setUrlArchivoInput(pathRelative);
		procesoAsincrono.setUrlArchivoOutput(null);
		procesoAsincrono.setJsonVariablesInput(requestDto.getJsonVariablesInput());
		procesoAsincrono.setJsonVariablesOutput(null);
		procesoAsincrono.setEstadoProcesoId(Constantes.ESTADO_PROCESO_ASINCR_PENDIENTE);
		procesoAsincrono.setCampoSegIns(jwt.getUsuario().getUsuario(), Instant.now());
		
		return procesoAsincrono;
	}

	@Override
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Transactional
	public void procesarCargaMasivaServidoresCiviles(ReqProcesoAsincrono request) {
		LOGGER.info("Procesando proceso de Carga Masiva de Servidores Civiles: procesoId={}, tipoProcesoId={}", request.getProcesoId(), request.getTipoProcesoId());
		
		Optional<Proceso> proceso = procesoRepository.findById(request.getProcesoId());
		
		if (!proceso.isPresent()) {
			LOGGER.error("El procesoId={} no fue encontrado...!!!", request.getProcesoId());
			return;
		}

		Proceso procesoAsincrono = proceso.get();
			
		// Actualizar estado del proceso a EN EJECUCION:
		procesoAsincrono.setEstadoProcesoId(Constantes.ESTADO_PROCESO_ASINCR_EN_EJECUCION);
		procesoAsincrono.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), "ADMIN", Instant.now());
		procesoRepository.save(procesoAsincrono);

		ExcelUtil<ServidorCivilExcelDTO> validacion = new ExcelUtil(ServidorCivilExcelDTO::new);
		RespBase<Object> response = new RespBase<>();
	       
		try {
			// Procesar las variables JSON de Input:
			Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(procesoAsincrono.getJsonVariablesInput(), Map.class);
 
			Long entidadId = Long.parseLong(parametros.get(Constantes.ENTIDADID).toString());

			LOGGER.info(">>>>> INICIO del proceso de Carga Masiva de Servidores Civiles. EntidadId = " + entidadId);
			
			String token = this.obtenerToken();
			if (token == null) {
				this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, "Error al obtener token", null, null, null);
				return;
			}

			RestTemplate restTemplate = new RestTemplate();
		    HttpHeaders headers = new HttpHeaders();
		    headers.setContentType(MediaType.APPLICATION_JSON);
	   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

	   	    ReqBase<String> fileUrl = new ReqBase<>();
			fileUrl.setPayload(procesoAsincrono.getUrlArchivoInput());
   	   	    HttpEntity<ReqBase<String>> requestFileDownload = new HttpEntity<>(fileUrl, headers);
   	   	    
   	   	    LOGGER.info("Ejecutando el servicio del API Maestra para obtener el archivo (downloadFileBase64)");
   	   	    ResponseEntity<String> respFileBase64 = restTemplate.postForEntity(this.uriBaseMaestra.concat(Constantes.ENDPOINT_BAJAR_ARCHIVO), requestFileDownload, String.class);
	   	    
	   	    if (respFileBase64.getStatusCode().compareTo(HttpStatus.OK) != 0) {
				this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, "Error al obtener archivo de la Carga Masiva de Servidores Civiles", null, null, null);
				LOGGER.error("Error al obtener archivo de la Carga Masiva de Servidores Civiles. Json={}", respFileBase64.getBody());
				return;
	   	    }
	   	    
   		    JsonNode root2 = objectMapper.readTree(respFileBase64.getBody());
   	   	    String base64 = root2.at("/payload").asText();
//   	   	    LOGGER.info("base64={}", base64);
			
			byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
			InputStream uploadedInputStream = new ByteArrayInputStream(bytes);
			InputStream uploadedInputStreamObserv = new ByteArrayInputStream(bytes);

			List<ServidorCivilExcelDTO> lista = this.obtenerListaServCivilfromExcel(uploadedInputStream);
			if (CollectionUtils.isEmpty(lista)) {
				LOGGER.error(Constantes.NO_SE_INGRESO_DATOS);
				ServidorCivilExcelDTO beanServidor = new ServidorCivilExcelDTO();
				beanServidor.setObservacionResultado(Constantes.NO_SE_INGRESO_DATOS);
				if (lista == null) {
					lista = new ArrayList<>();
				}
				lista.add(beanServidor);
				byte[] archivoObservado = adapterServidorCivil.excelObservacionServidorCivil(lista, uploadedInputStreamObserv);
				String fileObservadoBase64 = Base64.getEncoder().encodeToString(archivoObservado);
				String rutaArchivoError = subirArchivoErrores(procesoAsincrono, fileObservadoBase64);
				if (rutaArchivoError == null) {
					this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, "Error al cargar archivo observado", null, null, null);
					return;
				}
				LOGGER.info("Se genero y cargo el archivo observado al SFTP");
				this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, Constantes.NO_SE_INGRESO_DATOS, rutaArchivoError, lista, fileObservadoBase64);
			} else {
				
				MyJsonWebToken jwt = new MyJsonWebToken();
				jwt.usuario(procesoAsincrono.getUsuarioEnvio());

				response = this.validarListaServCivil(jwt, lista, entidadId);
				
				if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
					LOGGER.error(">>>>> ERROR en el proceso de Carga Masiva de Servidores Civiles. EntidadId = " + entidadId);
					LOGGER.error(response.getStatus().getError().getMessages().get(0));

					try {
						String fileBase64 = validacion.addObservacionesXLSXBase64(uploadedInputStreamObserv, lista,	true);

						if (lista != null) {
							for (int i = lista.size() - 1; i >= 0; i--) {
								if (StringUtils.isEmpty(lista.get(i).getObservacionResultado())) {
									lista.remove(i);
								} else {
									String observacion = "FILA " + (i + 2) + " : "
											+ lista.get(i).getObservacionResultado();
									lista.get(i).setFilaObservacion(observacion);
								}
							}
						}
						
						if (fileBase64.equals(Constantes.VACIO)) {
							this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, "Error al generar archivo de observaciones", null, null, null);
							return;
						} else {
							String rutaArchivoError = subirArchivoErrores(procesoAsincrono, fileBase64);
							if (rutaArchivoError == null) {
								this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, "Error al cargar archivo de observaciones", null, null, null);
								return;
							}
							LOGGER.info("Se genero y cargo el archivo de observaciones al SFTP");
							this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, "Se genero archivo de observaciones", rutaArchivoError, lista, fileBase64);
						}
					} catch (Exception e) {
						this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, e.getMessage(), null, null, null);
						LOGGER.error(e.getMessage(), e);
					}
				} else {
					LOGGER.info("FIN >>>>> Se realizo la Carga Masiva de Servidores Civiles SATISFACTORIAMENTE. EntidadId = " + entidadId);
					this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_OK, procesoAsincrono, null, null, null, null);
				}
			}
		} catch (Exception e) {
			this.procesarResultadoProceso(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR, procesoAsincrono, e.getMessage(), null, null, null);
			LOGGER.error(e.getMessage(), e);
		}
		
		LOGGER.info("Finalizando proceso de Carga Masiva de Servidores Civiles: procesoId={}, tipoProcesoId={}", request.getProcesoId(), request.getTipoProcesoId());
	}
	
	private void procesarResultadoProceso(Long estadoProcesoId, Proceso procesoAsincrono, String mensajeError, String rutaArchivoError, List<ServidorCivilExcelDTO> listaRegsError, String fileBase64) {
		if (estadoProcesoId.equals(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_OK)) {
			this.actualizaProcesadoOK(procesoAsincrono);
			this.notificarResultadoProceso(procesoAsincrono, "Se realizo la Carga Masiva de Servidores Civiles SATISFACTORIAMENTE", fileBase64);
		} else if (estadoProcesoId.equals(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR)) {
			this.actualizaProcesadoConError(procesoAsincrono, mensajeError, rutaArchivoError);
			if (listaRegsError != null && listaRegsError.size() > 0) {
				this.saveDetalleError(listaRegsError, procesoAsincrono.getProcesoId());
			}
			this.notificarResultadoProceso(procesoAsincrono, mensajeError, fileBase64);
		}
		
	}
	
	private void notificarResultadoProceso(Proceso procesoAsincrono, String msgResult, String fileBase64) {
		String token = this.obtenerToken();
		if (token == null) {
			LOGGER.error("Error al obtener token para notificar resultado del proceso");
			return;
		}

		RestTemplate restTemplate = new RestTemplate();
	    HttpHeaders headers = new HttpHeaders();
	    headers.setContentType(MediaType.APPLICATION_JSON);
   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

   	    HttpEntity<Object> requestSecurity = new HttpEntity<>(null, headers);
   	    
   	    Map<String, String> uriVariables = new HashMap<>();
   	    uriVariables.put("usuario", procesoAsincrono.getUsuarioEnvio());
 	
   	 	LOGGER.info("Ejecutando el servicio del API Seguridad para obtener los datos del usuario que envio el proceso");
   	    ResponseEntity<String> respUsuario = restTemplate.exchange(this.uriBaseSeguridad.concat(Constantes.ENDPOINT_BUSCAR_USUARIO_FILTRO_EQ).concat("?usuario={usuario}"), HttpMethod.GET, requestSecurity, String.class, uriVariables);

   	    if (respUsuario.getStatusCode().compareTo(HttpStatus.OK) == 0) {
   		    JsonNode root;
			try {
				root = objectMapper.readTree(respUsuario.getBody());
			    JsonNode itemsNode = root.path("payload").path("items");
			    String jsonArrayAsString = objectMapper.writeValueAsString(itemsNode);
			    List<RespApiSeguridadUsuario> usuariosList = objectMapper.readValue(jsonArrayAsString, new TypeReference<List<RespApiSeguridadUsuario>>() {});
				String email = null;
	   		    Long personaId = null;
			    for (RespApiSeguridadUsuario user : usuariosList) {
			    	email = user.getCorreoElectronico();
			    	personaId = user.getPersonaId();
			    	if (email != null) break;
			    }
	   		    
	   		    if ((email != null && !email.isEmpty()) && (personaId != null)) {

	   		   	    HttpEntity<Object> requestPersona = new HttpEntity<>(null, headers);
	   		   	    
	   		   	    Map<String, Long> uriVariables2 = new HashMap<>();
	   		   	    uriVariables2.put("personaId", personaId);
	   		 	
	   		   	 	LOGGER.info("Ejecutando el servicio del API Persona para obtener los datos de la persona que envio el proceso");
	   		   	    ResponseEntity<String> respPersona = restTemplate.exchange(this.uriBasePersona.concat(Constantes.ENDPOINT_OBTENER_PERSONA_BY_ID), HttpMethod.GET, requestPersona, String.class, uriVariables2);
	   		   	    
	   		   	    if (respPersona.getStatusCode().compareTo(HttpStatus.OK) == 0) {
   						root = objectMapper.readTree(respPersona.getBody());
   			   		    String nombres = root.path("payload").path("personaNatural").path("nombres").asText();
   			   		    String apellidoPaterno = root.path("payload").path("personaNatural").path("apellidoPaterno").asText();
   			   		    String nombreCompleto = nombres.concat(" ").concat(apellidoPaterno);
   			   		    Date fechaEnvio = Date.from(procesoAsincrono.getFechaEnvio());
   			   		    String patron = "dd-MM-yyyy 'a las' HH:MM:ss";
   			   		    SimpleDateFormat fechaSDF = new SimpleDateFormat(patron);
   			   		    String fechaEnvioStr = fechaSDF.format(fechaEnvio);
   			   		    ReqEmail reqEmail = adapToBeanEmailProcesoAsincrono(procesoAsincrono, email, nombreCompleto, fechaEnvioStr, msgResult, fileBase64);
   				    	ReqBase<ReqEmail> sendEmail = new ReqBase<>();
   						sendEmail.setPayload(reqEmail);
   				   	    HttpEntity<ReqBase<ReqEmail>> requestEmail = new HttpEntity<>(sendEmail, headers);

   				   	    LOGGER.info("Ejecutando el servicio del API Notificacion para enviar correo");
   			   	   	    ResponseEntity<String> responseEmail = restTemplate.postForEntity(this.uriBaseNotificacion.concat(Constantes.ENDPOINT_ENVIO_CORRO), requestEmail, String.class);
   				   	    
   				   	    if (responseEmail.getStatusCode().compareTo(HttpStatus.OK) == 0) {
   				   	    	LOGGER.info("Se envio la notificacion correctamente!!!!");
   				   	    } else {
   				   	    	LOGGER.error("No se pudo enviar la notificacion al correo del usuario");
   				   	    }

	   		   	    } else {
	   					LOGGER.error("No se pudo obtener los datos de la persona al cual notificar el resultado del proceso");
	   		   	    }
	   		    	
	   		    } else {
					LOGGER.error("No se pudo obtener el correo electronico y personaId al cual notificar el resultado del proceso");
	   		    }
	   		    
			} catch (Exception e) {
				LOGGER.error("Error al intentar notificar el resultado del proceso");
				LOGGER.error(e.getMessage());
			}
   	    } else {
			LOGGER.error("No se pudo obtener los datos del usuario al cual notificar el resultado del proceso");
   	    }
		
		
	}

	private ReqEmail adapToBeanEmailProcesoAsincrono(Proceso procesoAsincrono, String email, String nombreCompleto, String fechaEnvio, String msgResult, String fileBase64) {
		ReqEmail reqEmail =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		List<ReqEmailAttachment> attachments = new ArrayList<ReqEmailAttachment>();

		Map<String, Object> parametros =  new HashMap<>();
		parametros.put("NOMBRE_USUARIO", nombreCompleto);		
		parametros.put("NOMBRE_PROCESO", "Carga Masiva de Servidores Civiles GME");
		parametros.put("ID_PROCESO", procesoAsincrono.getProcesoId());
		parametros.put("FECHORA_PROCESO", fechaEnvio);
		parametros.put("MSG_RESULTADO", msgResult);

		dataEmail.setTemplateCode("CM_SRV_CIV_GME");
		dataEmail.setSubject(null);
		dataEmail.setTo(email);
		dataEmail.setBodyValues(parametros);
		
		if (fileBase64 != null && !fileBase64.isEmpty()) {
			reqEmail.setIncludeAttachments(true);
			ReqEmailAttachment att = new ReqEmailAttachment();
			String[] arrOfStr = procesoAsincrono.getUrlArchivoOutput().split("/");
			att.setFileName(arrOfStr[arrOfStr.length - 1]);
			att.setContent(fileBase64);
			attachments.add(att);
			dataEmail.setAttachments(attachments);
		} else {
			reqEmail.setIncludeAttachments(false);
		}
		
		reqEmail.setData(dataEmail);
		
		return reqEmail;
	}
	
	private void actualizaProcesadoConError(Proceso procesoAsincrono, String mensajeError, String rutaArchivoError) {
		procesoAsincrono.setUrlArchivoOutput(rutaArchivoError);
		procesoAsincrono.setEstadoProcesoId(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_ERROR);
		procesoAsincrono.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), "ADMIN", Instant.now());
		procesoAsincrono.setMensajeError(mensajeError);
		procesoAsincrono.setFechaFin(Instant.now());
		procesoRepository.save(procesoAsincrono);
	}

	private void actualizaProcesadoOK(Proceso procesoAsincrono) {
		procesoAsincrono.setEstadoProcesoId(Constantes.ESTADO_PROCESO_ASINCR_PROCESADO_OK);
		procesoAsincrono.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), "ADMIN", Instant.now());
		procesoAsincrono.setFechaFin(Instant.now());
		procesoRepository.save(procesoAsincrono);
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

	private List<ServidorCivilExcelDTO> obtenerListaServCivilfromExcel(InputStream uploadedInputStream) {
		try {
			ExcelUtil<ServidorCivilExcelDTO> file = new ExcelUtil<>(ServidorCivilExcelDTO::new);

			int[] hojasLeer = { Constantes.HOJA_EXCEL_CERO, Constantes.HOJA_EXCEL_DOS };
			List<ServidorCivilExcelDTO> listaServCivil = file.utilExcelToPojo(uploadedInputStream, hojasLeer);

			return listaServCivil;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);

		}
		return null;
	}

	private RespBase<Object> validarListaServCivil(MyJsonWebToken token, List<ServidorCivilExcelDTO> lista,
			Long entidadId) throws Exception {
		RespBase<Object> responseMasivo = new RespBase<>();
		Parametro limiteRegistro = generalRepository.buscarParametro(null, Constantes.LIMIT_MASIVO, null);

		ReqBase<BeanServidorCivilDTO> servidorCivilReq = new ReqBase<>();
		BeanServidorCivilDTO beanServidorCivilDTO = new BeanServidorCivilDTO();
		beanServidorCivilDTO.setEntidadId(entidadId);

		responseMasivo = verificaListaServCivil(token, lista, responseMasivo, limiteRegistro, servidorCivilReq, beanServidorCivilDTO);

		if(!responseMasivo.getStatus().getSuccess()) {
			responseMasivo.getStatus().setSuccess(Boolean.FALSE);
		}
		
		return responseMasivo;
	}

	private RespBase<Object> verificaListaServCivil(MyJsonWebToken token, List<ServidorCivilExcelDTO> lista,
			RespBase<Object> responseMasivo, Parametro limiteRegistro, ReqBase<BeanServidorCivilDTO> servidorCivilReq,
			BeanServidorCivilDTO beanServidorCivilDTO) throws Exception {
		RespBase<GenericResponseMessage> response = new RespBase<>();
		
		lista.removeIf(x -> !StringUtils.isEmpty(x.getEliminarFila()) && x.isFlagRegistrar() == false);
		if (limiteRegistro.getCodigoNumero() >= lista.size()) {

			List<Organigrama> lstOrganigrama = organigramaRepository.findByEntidadId(beanServidorCivilDTO.getEntidadId());
	
			if (!CollectionUtils.isEmpty(lstOrganigrama)) {

				boolean existeObs = false;
				for (ServidorCivilExcelDTO civExcelDTO : lista) {
					LOGGER.info(">>>>>>>>> VALIDANDO registro SC : " + civExcelDTO.toString());
					
					String obs = civExcelDTO.getObservacion();
					
					boolean isOKPuestoEnUO = false;
					
					if (StringUtils.isEmpty(obs)) {
						isOKPuestoEnUO = validarPuestoEnUO(beanServidorCivilDTO.getEntidadId(), civExcelDTO, lista);
						if (!isOKPuestoEnUO) {
							existeObs = true;
						}
						
					} else {
						existeObs = true;
					}
		
					if (StringUtils.isEmpty(obs) && civExcelDTO.isFlagRegistrar() && isOKPuestoEnUO) {
		
						Long organoId = Long.parseLong(civExcelDTO.getSiglaId().trim());
						Long regimenLaboralId = Long.parseLong(civExcelDTO.getRegimenId().trim());
						
						if (!lstOrganigrama.stream().anyMatch(o -> o.getOrganigramaId().equals(organoId))) {
							existeObs = true;
							civExcelDTO.agregarObservacion("El código del Organo / UO no esta registrado previamente");
						}
						
						long nroRepetidasDoc = lista.stream().filter(s -> s.getNumeroDocumento() != null && s.getNumeroDocumento().equals(civExcelDTO.getNumeroDocumento())).count();
						if (nroRepetidasDoc > 1) {
							existeObs = true;
							civExcelDTO.agregarObservacion("El número de documento se repite más de una vez en el archivo");
						}
						
						if (regimenLaboralId.equals(0L)) {
							existeObs = true;
							civExcelDTO.agregarObservacion("El código ID del Regimen Laboral no ha sido encontrado");
						}
					}
					
					if (StringUtils.isEmpty(obs)) {
						boolean existeObsValidacion = ejecutarValidacionCrearServidorCivil(civExcelDTO);
						if (existeObsValidacion) {
							existeObs = true;
						}
					}
				}
				
				if (!existeObs) {

					boolean huboProblemasCrearSC = false;
					for (ServidorCivilExcelDTO civExcelDTO : lista) {
						LOGGER.info(">>>>>>>>> GRABANDO registro SC : " + civExcelDTO.toString());

						Long organoId = Long.parseLong(civExcelDTO.getSiglaId().trim());
						Long regimenLaboralId = Long.parseLong(civExcelDTO.getRegimenId().trim());
						String sindicatoId = civExcelDTO.getSindicato().equals(Constantes.SELECCIONAR) ? "" : civExcelDTO.getSindicatoId().trim();
						
						ServidorCivilGDRDTO civGDRDTO = new ServidorCivilGDRDTO();
						
						civGDRDTO.setTipoDocumento(Integer.parseInt(civExcelDTO.getDocumentoId().trim()));
						civGDRDTO.setNumeroDocumento(civExcelDTO.getNumeroDocumento());
						civGDRDTO.setApellidoPaterno(civExcelDTO.getApellidoPaterno().toUpperCase());
						civGDRDTO.setApellidoMaterno(civExcelDTO.getApellidoMaterno().toUpperCase());
						civGDRDTO.setNombres(civExcelDTO.getNombres().toUpperCase());
						civGDRDTO.setCorreoElectronico(civExcelDTO.getCorreoLaboral().toLowerCase().trim());
						civGDRDTO.setSexo(civExcelDTO.getSexoId().trim());
						civGDRDTO.setPuestoDescripcion(civExcelDTO.getPuesto().toUpperCase());
						civGDRDTO.setPuestoId(Long.parseLong(civExcelDTO.getPuestoId().trim()));
						civGDRDTO.setFechaNacimiento(ParametrosUtil.StringToLocalDateDMY(civExcelDTO.getFechaNacimiento()));
						civGDRDTO.setFechaInicio(ParametrosUtil.StringToLocalDateDMY(civExcelDTO.getFechaInicio()));
						civGDRDTO.setOrganoId(organoId);
						civGDRDTO.setRegimenLaboralId(regimenLaboralId);
						civGDRDTO.setSindicatoId(sindicatoId);
						civGDRDTO.setResponsable(civExcelDTO.getResponsable().equalsIgnoreCase(Constantes.SI) ? "S" : "N");
						civGDRDTO.setTipoAsignacion(Constantes.TIPO_ASIGNACION_PRINCIPAL);

						beanServidorCivilDTO.setServidorCivil(civGDRDTO);
						servidorCivilReq.setPayload(beanServidorCivilDTO);
						
						try {
							response = servidorCivilService.crearServidorCivil(servidorCivilReq, token, true);
							
							if (Boolean.FALSE.equals(response.getStatus().getSuccess()) ) {
								huboProblemasCrearSC = true;
								String mensajeResultado = "";
								for (String mensaje : response.getStatus().getError().getMessages()) {
									mensajeResultado = mensajeResultado + mensaje + ",";
								}
								mensajeResultado = mensajeResultado.trim().substring(0, mensajeResultado.length() - 1);
//								civExcelDTO.setObservacionResultado(mensajeResultado);
								civExcelDTO.agregarObservacion(mensajeResultado);
							}
						} catch (Exception e) {
							System.out.println("<ERROR> Al ejecutar el metodo del servicio crearServidorCivil.");
							System.out.println(e.getMessage());
							huboProblemasCrearSC = true;
							civExcelDTO.agregarObservacion(e.getMessage());
						}

					}
					
					if (huboProblemasCrearSC) {
						responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE, "Hubo problemas al crear Servidor Civil en alguna fila del archivo");
						responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacionResultado().isEmpty()));
					} else {
						responseMasivo.setPayload(response.getPayload().getClass());
						responseMasivo.getStatus().setSuccess(Boolean.TRUE);
					}
					
				} else {
					LOGGER.info(">>>>>>>>> El archivo TIENE ERRORES, NO GRABARA REGISTROS DE SC ");
					responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE, "El archivo TIENE ERRORES, NO GRABARA REGISTROS DE SC ");
					responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacion().isEmpty()));
				}

			} else {
				responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
						"No existen Organos / Unidades Organicas registrados para esta entidad. ");
			}

		} else {
			responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
					Constantes.MENSAJE_LIMITE_MASIVO + limiteRegistro.getCodigoNumero());
		}

		return responseMasivo;
	}

	private boolean validarPuestoEnUO(Long entidadId, ServidorCivilExcelDTO servidorCivilExcelDTO, List<ServidorCivilExcelDTO> lista) {

		boolean pasoValidacion = true;
		
		if (servidorCivilExcelDTO.getSiglaId() == null) {
			pasoValidacion = false;
			servidorCivilExcelDTO.agregarObservacion("No se ingreso la sigla del Organo o UO");
			return pasoValidacion;
		}
		
		Long organigramaId = Long.valueOf(servidorCivilExcelDTO.getSiglaId());

		Optional<Puesto> valPuesto = puestoRepository.findByEntidadIdAndDescripcionAndOrganigramaIdAndEstadoRegistro(
				entidadId, servidorCivilExcelDTO.getPuesto().toUpperCase(), organigramaId, Constantes.ESTADO_ACTIVO);

		if (valPuesto.isPresent()) {
			String esJefe = valPuesto.get().getEsJefe();
			if (esJefe.equals(Constantes.ES_RESPONSABLE_S)) {
				
				if (lista.stream()
						.anyMatch(sc -> (sc.getNumeroDocumento() != null
								&& !sc.getNumeroDocumento().equals(servidorCivilExcelDTO.getNumeroDocumento()))
								&& ((sc.getPuesto() != null
										&& sc.getPuesto().equalsIgnoreCase(servidorCivilExcelDTO.getPuesto()))
										|| (sc.getPuestoId() != null
												&& sc.getPuestoId().equals(servidorCivilExcelDTO.getPuestoId()))))) {
					pasoValidacion = false;
					servidorCivilExcelDTO.agregarObservacion("Ya existe un servidor civil asignado para el puesto seleccionado en el mismo archivo");
				}				
				
				Optional<Organigrama> oOrganigrama = organigramaRepository.findById(organigramaId);
				if (oOrganigrama.isPresent()) {
					if (oOrganigrama.get().getPersonaResponsableId() != null) {
						pasoValidacion = false;
						servidorCivilExcelDTO.agregarObservacion("Ya existe un servidor civil asignado para el puesto seleccionado");
					}
				}
//				servidorCivilExcelDTO.setResponsable(Constantes.ES_RESPONSABLE_S);
			}
		} else {
			pasoValidacion = false;
			servidorCivilExcelDTO.agregarObservacion("El puesto NO pertenece a la UO de la entidad");
		}

		return pasoValidacion;
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private boolean ejecutarValidacionCrearServidorCivil(ServidorCivilExcelDTO civExcelDTO) {
		boolean existeObs = false;
		
		RespBase<ApiPersonaRequestDTO> request = adapterServidorCivil.adapToBeanPersona(civExcelDTO);
		
		try {
			String token = this.obtenerToken();
			if (token == null) {
				existeObs = true;
				civExcelDTO.agregarObservacion("Error al obtener token");
			} else {
				RestTemplate restTemplate = new RestTemplate();
			    HttpHeaders headers = new HttpHeaders();
			    headers.setContentType(MediaType.APPLICATION_JSON);
		   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

		   	    HttpEntity<RespBase<ApiPersonaRequestDTO>> requestValidarPersona = new HttpEntity<>(request, headers);

		   	    LOGGER.info("Ejecutando el servicio del API Persona para validar informacion de la Persona Natural");
	   	   	    ResponseEntity<String> responseValidarPersona = restTemplate.postForEntity(this.uriBasePersona.concat(Constantes.ENDPOINT_VAL_PERSONA_NATURAL), requestValidarPersona, String.class);
		   	    
		   	    if (responseValidarPersona.getStatusCode().compareTo(HttpStatus.OK) != 0) {
					existeObs = true;
					civExcelDTO.agregarObservacion("Hubo un error en API Persona al validar Persona Natural");
		   	    }
			}

		} catch (HttpClientErrorException e) {
			existeObs = true;
			if (e.getStatusCode().equals(HttpStatus.BAD_REQUEST)) {
				String error = e.getMessage().substring(e.getMessage().indexOf("{"), e.getMessage().length() - 1);
				try {
					RespBase<Object> respo = JsonUtil.convertirCadenaJsonPostAObjeto(error, RespBase.class);
					
					if (String.join(" ", respo.getStatus().getError().getMessages()).trim()
							.equalsIgnoreCase("documento ya existe")
							|| respo.getStatus().getError().getCode().trim().equalsIgnoreCase("CORREO")) {
						return false;
					}
					
					for (String msg : respo.getStatus().getError().getMessages()) {
						civExcelDTO.agregarObservacion(msg);
					}
					
				} catch (Exception e1) {
					civExcelDTO.agregarObservacion("Ocurrio un error al obtener mensaje del servicio (validarPersonaNatural). Message =".concat(e1.getMessage()));
					LOGGER.error("Ocurrio un error al obtener mensaje del servicio (validarPersonaNatural). Message =".concat(e1.getMessage()));
				}
			} else {
				civExcelDTO.agregarObservacion("Ocurrio un error al invocar el servicio del API Persona (validarPersonaNatural). Message =".concat(e.getMessage()));
			}
		} catch (Exception e) {
			existeObs = true;
			civExcelDTO.agregarObservacion("Ocurrio un error al invocar el servicio del API Persona (validarPersonaNatural). Message =".concat(e.getMessage()));
			LOGGER.error("Ocurrio un error al invocar el servicio del API Persona (validarPersonaNatural). Message =".concat(e.getMessage()));
		}
				
		return existeObs;
	}
	
	private String subirArchivoErrores(Proceso procesoAsincrono, String fileBase64) throws Exception {
		ReqRegistrarProcesoAsincrono requestDto = new ReqRegistrarProcesoAsincrono();
		requestDto.setTipoProcesoId(procesoAsincrono.getTipoProcesoId());
		requestDto.setBase64File(fileBase64);
		requestDto.setExtensionArchivo(Constantes.EXTENSION_FILE_CARGA_MAS_SERV_CIVIL);
		requestDto.setJsonVariablesInput(procesoAsincrono.getJsonVariablesInput());
		requestDto.setNombreArchivo(Constantes.PREFIJO_FILE_CARGA_MAS_SERV_CIVIL);
		requestDto.setUsuarioEnvio(procesoAsincrono.getUsuarioEnvio());

		String rutaFile = null;

		if (Strings.isNotEmpty(requestDto.getBase64File())) {
			String token = this.obtenerToken();
			if (token == null) {
				LOGGER.info("Error al obtener token");
				return null;
			}

			RestTemplate restTemplate = new RestTemplate();
		    HttpHeaders headers = new HttpHeaders();
		    headers.setContentType(MediaType.APPLICATION_JSON);
	   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

   	   	    HttpEntity<ReqBase<ApiFileServerDTO>> requestFileUpload = new HttpEntity<>(this.setearDatosArchivoRT(requestDto), headers);
   	   	    
   	   	    LOGGER.info("Ejecutando el servicio del API Maestra para cargar ul archivo al SFTP (/v2/file/upload)");
   	   	    ResponseEntity<String> respFileBase64 = restTemplate.postForEntity(this.uriBaseMaestra.concat(Constantes.ENDPOINT_INS_IMAGEN), requestFileUpload, String.class);
	   	    
	   	    if (respFileBase64.getStatusCode().compareTo(HttpStatus.OK) != 0) {
				LOGGER.error("Error al cargar archivo de OBSERVACIONES de la Carga Masiva de Servidores Civiles. Json={}", respFileBase64.getBody());
				return null;
	   	    }

   		    JsonNode root = objectMapper.readTree(respFileBase64.getBody());
   		    rutaFile = root.at("/payload/pathRelative").asText();
		}
		
		return rutaFile;
	}

	private ReqBase<ApiFileServerDTO> setearDatosArchivoRT(ReqRegistrarProcesoAsincrono requestDto) throws Exception {
		ReqBase<ApiFileServerDTO> request = new ReqBase<>();
		ApiFileServerDTO requestPayload = new ApiFileServerDTO();
		requestPayload.setFileBase64(requestDto.getBase64File());
		requestPayload.setExtension(requestDto.getExtensionArchivo());	// ".xlsm"
		requestPayload.setFileName(FilesUtil.generarNombreArchivoSinExt(requestDto.getNombreArchivo()));
		String pathFile = this.obtenerRutaFileAlfrescoRestTemplate(requestDto);
		requestPayload.setPath(pathFile);
		requestPayload.setRatioDeCambio(Constantes.RATIO_CAMBIO_GDR);
		requestPayload.setResize(false);
		request.setPayload(requestPayload);
		return request;
	}

	@SuppressWarnings({ "unchecked" })
	private String obtenerRutaFileAlfrescoRestTemplate(ReqRegistrarProcesoAsincrono requestDto) throws Exception {
		String ruta = "";

		String token = this.obtenerToken();
		if (token == null) {
			LOGGER.info("Error al obtener token");
			return null;
		}

		RestTemplate restTemplate = new RestTemplate();
	    HttpHeaders headers = new HttpHeaders();
	    headers.setContentType(MediaType.APPLICATION_JSON);
   	    headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + token);

   	    HttpEntity<Object> requestParam = new HttpEntity<>(null, headers);
   	    
   	    Map<String, String> uriVariables = new HashMap<>();
   	    uriVariables.put("tipoParametro", Constantes.CODIGO_ALFRESCO);
 	
   	 	LOGGER.info("Ejecutando el servicio del API Maestra para obtener Parametros ({})", Constantes.CODIGO_ALFRESCO);
   	    ResponseEntity<String> respParametro = restTemplate.exchange(this.uriBaseMaestra.concat(Constantes.ENDPOINT_TIPO_PARAMETRO), HttpMethod.GET, requestParam, String.class, uriVariables);
   	    
   	    if (respParametro.getStatusCode().compareTo(HttpStatus.OK) == 0) {
   		    JsonNode root = objectMapper.readTree(respParametro.getBody());
   		    JsonNode itemsNode = root.path("payload").path("items");
   		    String jsonArrayAsString = objectMapper.writeValueAsString(itemsNode);
   		    List<RespObtieneLista3.Parametro> paramRutaAlfresco = objectMapper.readValue(jsonArrayAsString, new TypeReference<List<RespObtieneLista3.Parametro>>() {});

   		    for (RespObtieneLista3.Parametro param : paramRutaAlfresco) {
				if (requestDto.getTipoProcesoId().equals(Constantes.TIPO_PROCESO_ASIN_CARGA_MASIVA_SERV_CIVIL) && 
						param.getCodigoTexto().equals(Constantes.RUTA_GME_CARGA_MAS_SERV_CIVIL)) {
					ruta = param.getValorTexto();
					break;
				}
			}
   	    }
		
		if (Strings.isNotEmpty(ruta)) {
			if (requestDto.getTipoProcesoId().equals(Constantes.TIPO_PROCESO_ASIN_CARGA_MASIVA_SERV_CIVIL)) {
				Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(requestDto.getJsonVariablesInput(), Map.class);
				String entidadId = parametros.get(Constantes.ENTIDADID).toString();
				
				ruta = ParametrosUtil.datePathReplaceRepositoryAlfresco(ruta);
				ruta = ruta.replace("{entidadId}", entidadId);
			}
		}
		
		return ruta;
	}
	
	private void saveDetalleError(List<ServidorCivilExcelDTO> lista, Long procesoId) {
		if (!lista.isEmpty()) {
			LOGGER.info("******************************** REGISTROS CON OBSERVACIONES ***************************************");
			for (ServidorCivilExcelDTO reg : lista) {
				LOGGER.info("Registro con OBS: {}", reg.toString());
				ProcesoError procesoError = new ProcesoError();
				procesoError.setProcesoId(procesoId);
				procesoError.setDetalleError(reg.toString());	
				procesoError.setCampoSegIns("ADMIN", Instant.now());
				procesoErrorRepository.save(procesoError);
			}
			LOGGER.info("****************************************************************************************************");
		}
	}

}