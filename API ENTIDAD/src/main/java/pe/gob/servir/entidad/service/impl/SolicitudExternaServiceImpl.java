package pe.gob.servir.entidad.service.impl;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

import lombok.SneakyThrows;
import org.apache.logging.log4j.util.Strings;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.stereotype.Service;

import org.springframework.web.multipart.MultipartFile;
import pe.gob.servir.entidad.adapter.BeanAdapterGestores;
import pe.gob.servir.entidad.adapter.BeanAdapterSolExtPerJuridica;
import pe.gob.servir.entidad.adapter.BeanAdapterSolicitudExt;
import pe.gob.servir.entidad.api.dto.*;
import pe.gob.servir.entidad.bean.ValidaExtensiones;
import pe.gob.servir.entidad.bean.ValidaGestor;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.feign.client.NotificacionApiClient;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.ComboDTO;
import pe.gob.servir.entidad.model.Encrypt;
import pe.gob.servir.entidad.model.Entidad;
import pe.gob.servir.entidad.model.EntidadTipoRucDTO;
import pe.gob.servir.entidad.model.Gestores;
import pe.gob.servir.entidad.model.ListaUsuarioRolEntidadDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.SolicitudExterna;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.model.UserRolEntidadDTO;
import pe.gob.servir.entidad.model.ValidarUsuarioDTO;
import pe.gob.servir.entidad.repository.CorreoRepositorySP;
import pe.gob.servir.entidad.repository.EntidadRepository;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.GestoresRepository;
import pe.gob.servir.entidad.repository.SolicitudExternaRepository;
import pe.gob.servir.entidad.repository.TelefonoRepositorySP;
import pe.gob.servir.entidad.repository.UsuarioRepositorySP;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqObservaSolicitudExt;
import pe.gob.servir.entidad.request.ReqRegistrarSolicitudExterna;
import pe.gob.servir.entidad.request.ReqValidaSolicitudExt;
import pe.gob.servir.entidad.response.*;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.service.SolicitudExternaService;
import pe.gob.servir.entidad.util.EncryptUtil;
import pe.gob.servir.entidad.util.FilesUtil;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class SolicitudExternaServiceImpl implements SolicitudExternaService{
	private static final Logger LOGGER = Logger.getLogger(SolicitudExternaServiceImpl.class);

	@Autowired
	private PersonaApiClient personaApiClient;
	
	@Autowired
	private SolicitudExternaRepository solicitudExternaRepository;
	
	@Autowired
	private EntidadRepository entidadRepository;
	
	@Autowired
	private BeanAdapterSolicitudExt beanAdapterSolicitudExt;

	@Autowired
	private MaestraApiClient maestraApiClient;

	@Autowired
	private NotificacionApiClient notificacionApiClient;

	@Autowired
	private VariablesSistema variablesSistema;
	
	@Autowired
	private GestionRepository gestionRepository;
	
	@Autowired
	private PersonaService personaService;
	
	@Autowired
	CorreoRepositorySP correoRepositorySP;
	
	@Autowired
	TelefonoRepositorySP telefonoRepositorySP;
	
	@Autowired
	private UsuarioRepositorySP usuarioRepositorySP;
	
	@Autowired
	private SeguridadApiClient seguridadApiClient;
	
	@Autowired
	private BeanAdapterGestores beanAdapterGestores;
	
	@Autowired
	private BeanAdapterSolExtPerJuridica beanAdapterSolExtPerJuridica;
	
	@Autowired
	private GeneralRepository generalRepositorySP;
	
	@Autowired
	private GestoresRepository gestoresRepository;
	
	@Override
	public RespBase<Object> buscarSolicitudEntidadExt(Integer tipoDocumento, String numeroDocumento) {
		RespSolicitudExt respSolicitudExt = new RespSolicitudExt();
		
		List<SolicitudExterna> listarSolicitudP = solicitudExternaRepository.findByListaRuc(numeroDocumento, Constantes.ESTADO_PENDIENTE_SOL);
		if ( listarSolicitudP.size() == 0 ) {
			List<EntidadTipoRucDTO> listEntidad = gestionRepository.buscarTipobyRucEntidad(tipoDocumento, numeroDocumento);
			if ( listEntidad.size() == 0 ) {
				RespBase<RespApiPersona> persona = personaApiClient.obtienePersonaPorDocumento(tipoDocumento, numeroDocumento);
				respSolicitudExt = beanAdapterSolicitudExt.adapToRespBaseSolicitudExt(persona);
				respSolicitudExt.setCantidadSol(listarSolicitudP.size());
				
				if(persona.getPayload().getPersonaJuridica() != null && persona.getPayload().getPersona() != null) {
					Entidad entidad = entidadRepository.findByEntidadPorIdPersona(persona.getPayload().getPersona().getPersonaId());			
					System.out.println("entidad:"+entidad);
					if(entidad!=null) {
						respSolicitudExt.setEntidadId(entidad.getEntidadId());
					}
					
				}

			} else {
				respSolicitudExt = beanAdapterSolicitudExt.adapToRespBaseEntidadTipoRucDTO(listEntidad.get(0));
				respSolicitudExt.setCantidadSol(listarSolicitudP.size());
			}
			
		} else {
			List<EntidadTipoRucDTO> listEntidad = gestionRepository.buscarTipobyRucEntidad(tipoDocumento, numeroDocumento);
			if ( listEntidad.size() == 0 ) {
				RespBase<RespApiPersona> persona = personaApiClient.obtienePersonaPorDocumento(tipoDocumento, numeroDocumento);
				respSolicitudExt = beanAdapterSolicitudExt.adapToApiPersonaSolicitudExt(persona, listarSolicitudP.get(0));
				respSolicitudExt.setCantidadSol(listarSolicitudP.size());
				
				if(persona.getPayload().getPersonaJuridica() != null && persona.getPayload().getPersona() != null) {
					Entidad entidad = entidadRepository.findByEntidadPorIdPersona(persona.getPayload().getPersona().getPersonaId());			
					if(entidad!=null) {
						respSolicitudExt.setEntidadId(entidad.getEntidadId());
					}
				}
				

			} else {
				respSolicitudExt = beanAdapterSolicitudExt.adapToRespBaseEntidadTipoRucDTO(listEntidad.get(0));
				respSolicitudExt.setCantidadSol(listarSolicitudP.size());
			}
		}
		
		return new RespBase<Object>().ok(respSolicitudExt);
	}

	@Override
	public RespBase<RespRegistrarSolicitudExterna> registrarSolicitudExterna(MyJsonWebToken jwt, ReqBase<ReqRegistrarSolicitudExterna> request) {

		RespBase<RespRegistrarSolicitudExterna> response = new RespBase<>();
		RespRegistrarSolicitudExterna responsePayload = new RespRegistrarSolicitudExterna();

		ReqRegistrarSolicitudExterna requestDto = request.getPayload();
		ValidaGestor validaGestor = this.validarGestor(requestDto);
		RespBase<RespApiFile> subirImagenLogo = null;
		String rutaImage = null;

		if (validaGestor.getCodigoRespuesta().equals(0L)) {

			if (Strings.isNotEmpty(requestDto.getBase64Image())) {
				subirImagenLogo = this.maestraApiClient.insertImagen(this.setearDatosImagen(requestDto));
				rutaImage = Boolean.TRUE.equals(subirImagenLogo.getStatus().getSuccess()) &&
						    Strings.isNotEmpty(subirImagenLogo.getPayload().getPathRelative()) ? subirImagenLogo.getPayload().getPathRelative() : "";
			}

			SolicitudExterna solicitudExterna =
					this.setearSolicitudExterna(requestDto, rutaImage, jwt);
			solicitudExterna.setRutaArchivo(requestDto.getRutaArchivo());
			this.solicitudExternaRepository.save(solicitudExterna);

			ReqEmail setarMail = this.setarDatoCorreoRegSolExt(requestDto);
			ReqBase<ReqEmail> requestMail = new ReqBase<>();
			requestMail.setPayload(setarMail);

			this.notificacionApiClient.enviarCorreo(requestMail);

			responsePayload.setSolicitudEntidadExtId(solicitudExterna.getSolicitudEntidadExtId());
			responsePayload.setCodigoRespuesta(0L);
			responsePayload.setMensajeRespuesta("Operacion Exitosa");
			response.setPayload(responsePayload);
			response.getStatus().setSuccess(Boolean.TRUE);

		} else {

			if (validaGestor.getCodigoRespuesta() < 0) {
				RespBase.Status.Error error = new RespBase.Status.Error();
				List<String> msjError = new ArrayList<>();
				msjError.add(validaGestor.getMensajeRespuesta());
				error.setMessages(msjError);
				response.getStatus().setError(error);
				response.getStatus().setSuccess(Boolean.FALSE);
			}

			if (validaGestor.getCodigoRespuesta() > 0) {
				responsePayload.setCodigoRespuesta(validaGestor.getCodigoRespuesta());
				responsePayload.setMensajeRespuesta(validaGestor.getMensajeRespuesta());

				response.setPayload(responsePayload);
				response.getStatus().setSuccess(Boolean.FALSE);
			}


		}

		return response;
	}

	private ValidaGestor validarGestor (ReqRegistrarSolicitudExterna requestDto) {
		ValidaGestor validaGestor = new ValidaGestor();

		if (Constantes.TIPO_DOCUMENTO_CE.equals(requestDto.getTipoDocumento().longValue())) {
			validaGestor.setCodigoRespuesta(0L);
			validaGestor.setMensajeRespuesta("Validacion Correcta.");
		} else {

			try {
				
				RespBase<RespApiPersona> persona = this.personaApiClient.obtienePersonaPorDocumentoReniec(requestDto.getTipoDocumento(), requestDto.getNumeroDocumento());
				LOGGER.info("[servicio obtienePersonaPorDocumento fecha nacimiento: ] " + JsonUtil.convertirObjetoACadenaJson(persona.getPayload().getPersonaNatural().getFechaNacimiento()));
				boolean validarPersona = Boolean.TRUE.equals(persona.getStatus().getSuccess()) && Objects.nonNull(persona.getPayload().getPersonaNatural());
				if (validarPersona) {
					LOGGER.info("[requestDto lo que envia front: ] " + requestDto.getFechaNacimiento());
					String validarDatosPersona = this.validarDatosPersona(requestDto, persona.getPayload());
					LOGGER.info("[validarDatosPersona: ] " + JsonUtil.convertirObjetoACadenaJson(validarDatosPersona));
					if(Strings.isNotEmpty(validarDatosPersona)){
						validaGestor.setCodigoRespuesta(-2L);
						validaGestor.setMensajeRespuesta("[ " + validarDatosPersona + " ]");
					} else {
						boolean validarSolicitudesExistentes = this.validarSolicitudesGestor(requestDto);

						if (validarSolicitudesExistentes) {
							validaGestor.setCodigoRespuesta(0L);
							validaGestor.setMensajeRespuesta("Validacion Correcta.");
						} else {
							validaGestor.setCodigoRespuesta(2L);
							validaGestor.setMensajeRespuesta("[ El gestor tiene solicitudes pendientes. ]");
						}
					}

				} else {
					validaGestor.setCodigoRespuesta(1L);
					validaGestor.setMensajeRespuesta("[ El gestor no se pudo validar en Reniec. ]");
				}
			} catch (Exception e) {
				validaGestor.setCodigoRespuesta(-1L);
				validaGestor.setMensajeRespuesta(this.obtenerMensajeError(e.getMessage()));
			}


		}

		return validaGestor;
	}

	private String validarDatosPersona(ReqRegistrarSolicitudExterna requestDto, RespApiPersona persona) {
		String messageValidate = "";
		String strDate = "";
		if(null != persona.getPersonaNatural().getFechaNacimiento()) {
			String[] fecha = persona.getPersonaNatural().getFechaNacimiento().split("-");
			strDate = fecha[2].concat("/" + fecha[1]).concat("/" + fecha[0]);
		}
		LOGGER.info("[fecha se hace un replace al - por /: ] " + strDate);
		if(!requestDto.getApellidoPaterno().toUpperCase().equals(persona.getPersonaNatural().getApellidoPaterno().toUpperCase())){
			messageValidate = messageValidate + "- Apellido Paterno, no es correcto.";
		}  
		if(!requestDto.getApellidoMaterno().toUpperCase().equals(persona.getPersonaNatural().getApellidoMaterno().toUpperCase())) {
			messageValidate = messageValidate + "- Apellido Materno, no es correcto.";
		}
		if(!requestDto.getNombres().toUpperCase().equals(persona.getPersonaNatural().getNombres().toUpperCase())) { 
			messageValidate = messageValidate + "- Nombres, no es correcto.";
		}
		if(!requestDto.getFechaNacimiento().equals(strDate)) {
			LOGGER.info("[fecha no son iguales]");
			messageValidate = messageValidate + "Fecha de nacimiento, no es correcto.";
		}

		return messageValidate;
	}

	private boolean validarSolicitudesGestor (ReqRegistrarSolicitudExterna requestDto) {
		boolean validar = true;

		SolicitudExterna filtro = new SolicitudExterna();
		filtro.setNumeroDocumento(requestDto.getNumeroDocumento());
		filtro.setRucEntidad(requestDto.getRucEntidad());
		Example<SolicitudExterna> example = Example.of(filtro);

		List<SolicitudExterna> lstSolicitudesExternas = this.solicitudExternaRepository.findAll(example);

		if (!lstSolicitudesExternas.isEmpty()) {

			for (SolicitudExterna sol : lstSolicitudesExternas) {
				if ((!sol.getEstadoSolicitud().equals(4) && !sol.getEstadoSolicitud().equals(2))) {
					validar = false;
					break;
				}
			}
		}
		return validar;
	}

	private ReqBase<ApiFileServerDTO>  setearDatosImagen (ReqRegistrarSolicitudExterna requestDto) {
		ReqBase<ApiFileServerDTO> request = new ReqBase<>();
		ApiFileServerDTO requestPayload = new ApiFileServerDTO();
		String[] array = requestDto.getBase64Image().split(",");
		String base = array[array.length-1];
		requestPayload.setFileBase64(base);
		requestPayload.setExtension(FilesUtil.verificarExtensionImagen(requestDto.getBase64Image()));
		requestPayload.setFileName(FilesUtil.generarNombreArchivoSinExt("logos"));
		String pathImage = this.obtenerRutaImageAlfresco(requestDto);
		requestPayload.setPath(pathImage);
		requestPayload.setRatioDeCambio(Constantes.RATIO_CAMBIO_GDR);
		requestPayload.setResize(true);
		request.setPayload(requestPayload);
		return request;
	}


	private String obtenerRutaImageAlfresco (ReqRegistrarSolicitudExterna requestDto) {
		String ruta = "";
		RespBase<RespObtieneLista> paramRutaAlfresco = this.maestraApiClient.obtieneParametros(Constantes.CODIGO_ALFRESCO);

		if (Boolean.TRUE.equals(paramRutaAlfresco.getStatus().getSuccess()) ) {
			for (RespObtieneLista.Parametro param: paramRutaAlfresco.getPayload().getItems()) {
				if (param.getCodigoTexto().equals(Constantes.RUTA_GDR_FILES_MAESTRA)) {
					ruta = param.getValorTexto();
					break;
				}
			}
		}
		if (Strings.isNotEmpty(ruta)) {
			ruta = ParametrosUtil.datePathReplaceRepositoryAlfresco(ruta);
			ruta = ruta.replace("{ruc}", requestDto.getRucEntidad());
		}
		return ruta;
	}

	private SolicitudExterna setearSolicitudExterna (ReqRegistrarSolicitudExterna requestDto,
													 String pathRelative, MyJsonWebToken jwt) {
		SolicitudExterna solicitudExterna = new SolicitudExterna();
		solicitudExterna.setRucEntidad(requestDto.getRucEntidad());
		solicitudExterna.setRazonSocial(requestDto.getRazonSocial());
		solicitudExterna.setAbreviatura(requestDto.getAbreviatura().toUpperCase());
		solicitudExterna.setNombreEntidad(requestDto.getNombreEntidad());
		solicitudExterna.setNivelGobiernoId(requestDto.getNivelGobiernoId());
		solicitudExterna.setNivelGobierno(requestDto.getNivelGobierno());
		solicitudExterna.setSectorId(requestDto.getSectorId());
		solicitudExterna.setTipoEntidadId(requestDto.getTipoEntidadId());
		solicitudExterna.setTipoEntidad(requestDto.getTipoEntidad());
		solicitudExterna.setUrlLogoEntidad(Strings.isNotEmpty(pathRelative) ?  pathRelative : null);
		solicitudExterna.setTipoDocumento(requestDto.getTipoDocumento());
		solicitudExterna.setNumeroDocumento(requestDto.getNumeroDocumento());
		solicitudExterna.setApellidoPaterno(requestDto.getApellidoPaterno());
		solicitudExterna.setApellidoMaterno(requestDto.getApellidoMaterno());
		solicitudExterna.setNombres(requestDto.getNombres());
		solicitudExterna.setFechaNacimiento(requestDto.getFechaNacimiento());
		solicitudExterna.setTelefonoFijo(requestDto.getTelefonoFijo());
		solicitudExterna.setAnexo(requestDto.getAnexo());
		solicitudExterna.setCelular(requestDto.getCelular());
		solicitudExterna.setCorreoElectronico(requestDto.getCorreoElectronico());
		solicitudExterna.setUuidDocumento(requestDto.getUuId());
		solicitudExterna.setEstadoSolicitud(1);
		solicitudExterna.setCampoSegIns(jwt.getUsuario().getUsuario(), Instant.now());
		solicitudExterna.setSindicato(requestDto.getSindicato());
		solicitudExterna.setCorreoGestorGdr(requestDto.getCorreoElectronicoGestor());
		return solicitudExterna;
	}

	private ReqEmail setarDatoCorreoRegSolExt(ReqRegistrarSolicitudExterna requestDto) {
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		parametros.put("NOMBRE_USUARIO", requestDto.getNombres() + " " + requestDto.getApellidoPaterno() + " " + requestDto.getApellidoMaterno());
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		dataEmail.setTemplateCode(Constantes.REG_SOL_ENT_EXT);
		dataEmail.setSubject(null);
		dataEmail.setTo(requestDto.getCorreoElectronico());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		return email;
	}

	private String obtenerMensajeError (String msjError) {
		String messages = "messages";
		String payload = "payload";
		return msjError.substring(msjError.indexOf(messages), msjError.indexOf(payload));
	}

	@Override
	public RespBase<RespListarSolicitudExterna> filtrarSolicitudExterna(Map<String, Object> parametroMap) {
		LOGGER.info("Metodo filtrarSolicitudExterna...");
		List<SolicitudExternaDTO> listaDTO = gestionRepository.filtrarSolicitudExterna(parametroMap);
		RespListarSolicitudExterna respPayload = new RespListarSolicitudExterna();
		respPayload.setSolicitudExternaDto(listaDTO);
		
		return new RespBase<RespListarSolicitudExterna>().ok(respPayload);
	}

	@Override
	public RespBase<SolicitudExternaDTO> getSolicitudExternaId(Long solicitudExternaId) {
		LOGGER.info("Metodo getSolicitudExternaId...");
		RespBase<SolicitudExternaDTO> response = new RespBase<SolicitudExternaDTO>();
		Map<String, Object> parametroMap = new HashMap<String, Object>();
		parametroMap.put("solicitudExtId", solicitudExternaId);
		List<SolicitudExternaDTO> listaDTO = gestionRepository.filtrarSolicitudExterna(parametroMap);
		SolicitudExternaDTO solicitudExtDTO = new SolicitudExternaDTO();
		if(!listaDTO.isEmpty()) {
			solicitudExtDTO = listaDTO.get(0);
			response.getStatus().setSuccess(true);
		} else {
			solicitudExtDTO = null;
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add("No se encontro solicitud Externa");
		}
		response.setPayload(solicitudExtDTO);
		return response;
	}

	@Override
	public RespBase<List<ComboDTO>> getAnioSolExt() {
		LOGGER.info("Metodo getAnioSolExt...");
		RespBase<List<ComboDTO>> response = new RespBase<List<ComboDTO>>();
		 List<ComboDTO> listComboDTO = this.gestionRepository.listaAnioSolExt();
		 return response.ok(listComboDTO);
	}

	@SuppressWarnings("null")
	@Override
	public RespBase<Object> aceptarSolicitudExterna(MyJsonWebToken token, Long solicitudExtId, ReqBase<ReqValidaSolicitudExt> request) {
		RespBase<Object> response = new RespBase<>();
		try {
			Map<String, Object> parametroMap = new HashMap<String, Object>();
			parametroMap.put("solicitudExtId", solicitudExtId);
			LOGGER.debug("Valida Solicitud Externa"); 
			List<SolicitudExternaDTO> listaDTO = gestionRepository.filtrarSolicitudExterna(parametroMap);
			if (!listaDTO.isEmpty()) {
				SolicitudExternaDTO solicitud = listaDTO.get(0);
				SolicitudExternaDTO solicitud2 = null;
				 
				if( solicitud.getEstadoId().intValue() == 1) {
					LOGGER.debug("Inicia el proceso de validar persona juridica");
					RespBase<Object> perJuridica = creaPersonaJurBySolicitudExterna(solicitud, token);
					if (perJuridica.getStatus().getSuccess().equals(Boolean.FALSE)) {
						response.setPayload(null);
						return response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR AL INSERTAR PERSONA JURIDICA : " +
								perJuridica.getStatus().getError().getMessages().get(0));
						
					}
					
					Entidad oEntidad = (Entidad) perJuridica.getPayload();
					List<SolicitudExternaDTO> listaDTO2 = gestionRepository.filtrarSolicitudExterna(parametroMap);
					if (!listaDTO2.isEmpty()) {
						solicitud2 = listaDTO2.get(0);
					}else {
						solicitud2 = listaDTO.get(0);
					}
					

					LOGGER.debug("Inicia el proceso deshabilitar perfil Jefe ORH :");
					
					LOGGER.debug("Buscamos :");
					
					System.out.println("procceeso actualizar orh");
					RespBase<Object> getJefeORH = searchAndDeleteJefeORHByEntidadEstado(solicitud, token, oEntidad);
				
					if (getJefeORH.getStatus().getSuccess().equals(Boolean.FALSE)) {
						response.setPayload(null);
						return response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR AL DESASOCIAR PERSONA NATURAL POR ENTIDAD : " +
								getJefeORH.getStatus().getError().getMessages().get(0));
						
					}
					
					System.out.println("Inicia el proceso de validar persona natural");
					
					LOGGER.debug("Inicia el proceso de validar persona natural");
					RespBase<Object> perNatural =  registraOrUpdateJefeORH(solicitud, token, oEntidad);
					if (perNatural.getStatus().getSuccess().equals(Boolean.FALSE)) {
						response.setPayload(null);
						return response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR AL REGISTRAR O ACTUALIZAR PERSONA NATURAL POR ENTIDAD : " + 
						perNatural.getStatus().getError().getMessages().get(0));
						
					}
					
					Map<String, Object> solicitudAprobada = new HashMap<String, Object>();
					parametroMap.put("perJuridica", perJuridica.getPayload());
					parametroMap.put("perNatural", perNatural.getPayload());
					
					
					LOGGER.debug("Inicia el proceso de validar e envio de correo");
					if (perJuridica.getStatus().getSuccess().equals(Boolean.TRUE) 
							&& perNatural.getStatus().getSuccess().equals(Boolean.TRUE)) {
						LOGGER.debug("Busca entidad para actualizar la entidad");
						
						if (solicitud.getCorreoElectronico() != null ) {
							System.out.println("paseeeeeeeeeeee 888888888:");
							ReqEmail setarMail = new ReqEmail();
							setarMail = beanAdapterSolicitudExt.adapToCorreoSolExtValidado(solicitud, solicitud.getCorreoElectronico());
							ReqBase<ReqEmail> requestMail = new ReqBase<>();
							requestMail.setPayload(setarMail);

							this.notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
						}
						
						
						if (solicitud.getCorreoGestorGdr() != null ) {
							
							System.out.println("paseeeeeeeeeeee 777777:");
							ReqEmail setarMail = new ReqEmail();
							setarMail = beanAdapterSolicitudExt.adapToCorreoSolExtValidado(solicitud, solicitud.getCorreoGestorGdr());
							ReqBase<ReqEmail> requestMail = new ReqBase<>();
							requestMail.setPayload(setarMail);

							this.notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
						}
						 
						
					LOGGER.debug("Actualiza Solicitud Externa");
					Optional<SolicitudExterna> solicitudExt = solicitudExternaRepository.findById(solicitud.getSolicitudEntidadExtId());
					SolicitudExterna soli = solicitudExt.isPresent() ? solicitudExt.get() : new SolicitudExterna();
					soli.setEstadoSolicitud(4);
					solicitudExternaRepository.save(soli);
					
					response.setPayload(solicitudAprobada);
					response.getStatus().setSuccess(Boolean.TRUE);
					} else {
						response.setPayload(null);
						return response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR AL PROCESAR LA SOLICITUD EXTERNA");
						
					}
						
				
				} else if(solicitud.getEstadoId().intValue() == 2) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"LA SOLICITUD YA SE ENCUENTRA OBSERVADA");
					response.setPayload(solicitud);
					return response;
				} else if(solicitud.getEstadoId().intValue() == 3) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"LA SOLICITUD YA SE ENCUENTRA RECHAZADA");
					response.setPayload(solicitud);
					return response;
				} else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"LA SOLICITUD YA SE ENCUENTRA APROBADA");
					response.setPayload(solicitud);
					return response;
				}
			}
		}catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;
	}
			
	public Boolean validaSolicitud(SolicitudExternaDTO solicitudExt) {
		Boolean valido = true;
		/*if( solicitudExt.getNombreEntidad() == null) {
			valido = false;
		}
		*/
		if( solicitudExt.getRazonSocial() == null) {
			valido = false;
		}
		if( solicitudExt.getRucEntidad() == null) {
			valido = false;
		}
		return valido;
	}
				
	@SuppressWarnings({ "rawtypes"})
	public RespBase<Object> creaPersonaJurBySolicitudExterna (SolicitudExternaDTO solicitudExt, MyJsonWebToken token) {
		RespBase<Object> response = new RespBase<Object>();
		try {
			RespBase<RespApiPersona> persona = personaApiClient.obtienePersonaPorDocumento(Constantes.TIPO_DOCUMENTO_RUC, solicitudExt.getRucEntidad());
			
			if (persona.getPayload().getPersonaJuridica() == null) {
				response.setPayload(solicitudExt);
				return response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "PERSONA JURIDICA NO ENCONTRADA");
			} 
			Boolean valido = validaSolicitud(solicitudExt);
			if ( !valido ) {
				response.setPayload(solicitudExt);
				return response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "SOLICITUD CON DATOS INCOMPLETOS Y/O ERRADOS");
			}
			
			if (persona.getPayload().getPersonaJuridica().getPersonaId() == null) {
				LOGGER.debug("Busca datos para crear la entidad");
				System.out.println("Busca datos para crear la entidad");
				RespBase<ApiPersonaRequestDTO> personaJuridicaResquest = new RespBase<>();
				ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaJuridica> apiPersonaJuridica = new ApiPersonaRequestDTO<>();
				//ParametrosUtil.setearPersonaJuridica(apiPersonaJuridica, listaPersonaJuridica.get(0),variablesSistema.tipoDocumentoRuc);
				apiPersonaJuridica = beanAdapterSolExtPerJuridica.adapSolicitudExtToJuridico(persona, solicitudExt, variablesSistema.tipoDocumentoRuc);
				personaJuridicaResquest.setPayload(apiPersonaJuridica);
				response = personaService.obtenerInsertarPersona(variablesSistema.tipoPersonaJuridico, variablesSistema.tipoDocumentoRuc, solicitudExt.getRucEntidad(), personaJuridicaResquest);
				Entidad entidad = null; 
				Long personaJuridicaId = null;
				if (Boolean.TRUE.equals(response.getStatus().getSuccess())) {
					
					System.out.println("crear persona ");
					RespApiPersona beanPersonaJuridica = (RespApiPersona) response.getPayload();
					personaJuridicaId = beanPersonaJuridica.getPersona().getPersonaId();
					Entidad entidadBusqueda = new Entidad();
					entidadBusqueda.setPersonaId(personaJuridicaId);
					entidadBusqueda.setTipoEntidadId(variablesSistema.tipoPersonaJuridico);
					entidadBusqueda.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
					Example<Entidad> exampleEntidad = Example.of(entidadBusqueda);
					List<Entidad> listaEntidad = entidadRepository.findAll(exampleEntidad);
					Parametro parametro = generalRepositorySP.buscarParametro(null, null,Constantes.RUTA_FILE_SERVER_ENTIDAD);
					String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);
				
					if (!listaEntidad.isEmpty()) {
						System.out.println("crear persona listaEntidad.isEmpty()");
						
						entidad = listaEntidad.get(0);
						entidad.setFlagActualiza(Constantes.FLAG_NO_ACTUALIZO_ENTIDAD);
						System.out.println("crear persona listaEntidad.isEmpty( 2)");
						entidad.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
						entidad.setUrlLogoEntidad(solicitudExt.getUrlLogoEntidad());
						entidad.setSigla(solicitudExt.getAbreviatura());							
						entidad.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),Instant.now());
						entidadRepository.save(entidad);
						System.out.println("save persona listaEntidad.isEmpty( 3)");
						rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD, entidad.getEntidadId().toString());
						//ACTUALIZA ENTIDAD
						LOGGER.debug("Actualiza entidadId a la Solicitud Externa");
						SolicitudExterna solicitudExterna = solicitudExternaRepository.findBySolExtId(solicitudExt.getSolicitudEntidadExtId());
						if ( solicitudExterna != null ) {
							solicitudExterna.setEntidadId(entidad.getEntidadId());
							solicitudExternaRepository.save(solicitudExterna);
							LOGGER.debug("termino de actualizar");
						}
					} else {
						
						System.out.println(" no existe persona 1");
						entidad = new Entidad();
						entidad.setPersonaId(personaJuridicaId);
						System.out.println(" no existe persona 2");
						entidad.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
						entidad.setUbigeoId((beanPersonaJuridica.getDirecciones() != null && !beanPersonaJuridica.getDirecciones().isEmpty())	? beanPersonaJuridica.getDirecciones().get(0).getUbigeoId()	: null);
						System.out.println(" no existe persona 3");
						entidad.setNivelGobiernoId(solicitudExt.getNivelGobiernoId());
						entidad.setSectorId(solicitudExt.getSectorId());
						System.out.println(" no existe persona 4");
						entidad.setTipoEntidadId(variablesSistema.tipoPersonaJuridico);
						System.out.println(" no existe persona 5");
						entidad.setTipoEntidadPubId((solicitudExt.getTipoEntidadId() != null)	? Integer.valueOf(solicitudExt.getTipoEntidadId().toString())	: null);
						entidad.setFlagActualiza(Constantes.FLAG_NO_ACTUALIZO_ENTIDAD);
						entidad.setDescripcionEntidad(beanPersonaJuridica.getPersonaJuridica().getRazonSocial());
						entidad.setUrlWebEntidad((beanPersonaJuridica.getWebs() != null && !beanPersonaJuridica.getWebs().isEmpty())? beanPersonaJuridica.getWebs().get(0).getUrlWeb(): null);
						if ((solicitudExt.getUrlLogoEntidad() != null) && (!solicitudExt.getUrlLogoEntidad().isEmpty())) {
							entidad.setUrlLogoEntidad(solicitudExt.getUrlLogoEntidad());
						}
						System.out.println(" no existe persona 6");
						
						entidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
						entidad.setSigla(solicitudExt.getAbreviatura());
	                	entidad.setNroSindicatos(solicitudExt.getSindicato());								

						System.out.println(" no existe persona 7");
						entidadRepository.save(entidad);
						rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,entidad.getEntidadId().toString());
						//ACTUALIZA ENTIDAD
						LOGGER.debug("Actualiza entidadId a la Solicitud Externa");
						System.out.println(" no existe persona 8");
						SolicitudExterna solicitudExterna = solicitudExternaRepository.findBySolExtId(solicitudExt.getSolicitudEntidadExtId());
						System.out.println(" no existe persona 9");
						if ( solicitudExterna != null ) {
							solicitudExterna.setEntidadId(entidad.getEntidadId());
							solicitudExternaRepository.save(solicitudExterna);
						}

						response.setPayload(entidad);
						response.getStatus().setSuccess(Boolean.TRUE);
					}
					
				} else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR AL INSERTAR PERSONA JURIDICA");
					response.setPayload(solicitudExt);
				}
				
			} else {
				LOGGER.debug("Busca datos para actualizar la entidad");
				System.out.println("llegue a actualizar");
				// actualiza entidad
				RespBase<ApiActualizarPersonaJuridica> perJuridica = new RespBase<>();
				ApiActualizarPersonaJuridica juridica = new ApiActualizarPersonaJuridica();
				juridica.setPersonaId(persona.getPayload().getPersonaJuridica().getPersonaId());
				juridica.setNombreComercial(persona.getPayload().getPersonaJuridica().getNombreComercial());
				juridica.setRazonSocial(persona.getPayload().getPersonaJuridica().getRazonSocial());
				juridica.setRuc(persona.getPayload().getDocumentos().get(0).getNumeroDocumento());
				perJuridica.setPayload(juridica);
				RespBase<RespApiPersona> personaJur = 
						personaApiClient.actualizaPersonaJuridica(persona.getPayload().getPersonaJuridica().getPersonaId(), perJuridica);
				System.out.println("pase1");
				Entidad entidad = null;
				Long personaJuridicaId = null;
				if (Boolean.TRUE.equals(personaJur.getStatus().getSuccess())) {
					
					System.out.println("Boolean.TRU");
					RespApiPersona beanPersonaJuridica = (RespApiPersona) personaJur.getPayload();
					personaJuridicaId = beanPersonaJuridica.getPersona().getPersonaId();
					Entidad entidadBusqueda = new Entidad();
					entidadBusqueda.setPersonaId(personaJuridicaId);
					entidadBusqueda.setTipoEntidadId(variablesSistema.tipoPersonaJuridico);
					entidadBusqueda.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
					Example<Entidad> exampleEntidad = Example.of(entidadBusqueda);
					List<Entidad> listaEntidad = entidadRepository.findAll(exampleEntidad);
					Parametro parametro = generalRepositorySP.buscarParametro(null, null,Constantes.RUTA_FILE_SERVER_ENTIDAD);
					String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);
					if (!listaEntidad.isEmpty()) {
						System.out.println("listaEntidad.isEmpty()");
						entidad = listaEntidad.get(0);
						entidad.setFlagActualiza(Constantes.FLAG_NO_ACTUALIZO_ENTIDAD);
						entidad.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
						entidad.setSigla(solicitudExt.getAbreviatura());							

						entidad.setUrlLogoEntidad(solicitudExt.getUrlLogoEntidad());
						entidad.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),Instant.now());
						entidadRepository.save(entidad);
						rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD, entidad.getEntidadId().toString());
						//ACTUALIZA ENTIDAD
						LOGGER.debug("Actualiza entidadId a la Solicitud Externa");
						SolicitudExterna solicitudExterna = solicitudExternaRepository.findBySolExtId(solicitudExt.getSolicitudEntidadExtId());
						if ( solicitudExterna != null ) {
							solicitudExterna.setEntidadId(entidad.getEntidadId());
							solicitudExternaRepository.save(solicitudExterna);
						}
						response.setPayload(entidad);
						response.getStatus().setSuccess(Boolean.TRUE);
					} else {
						entidad = new Entidad();
						entidad.setPersonaId(personaJuridicaId);
						entidad.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
						System.out.println("listaEntidad. lleo()");
						entidad.setUbigeoId((beanPersonaJuridica.getDirecciones() != null && !beanPersonaJuridica.getDirecciones().isEmpty())	? beanPersonaJuridica.getDirecciones().get(0).getUbigeoId()	: null);
						System.out.println("paseeeeeeeee");
						entidad.setNivelGobiernoId(solicitudExt.getNivelGobiernoId());
						entidad.setSectorId(solicitudExt.getSectorId());
						entidad.setTipoEntidadId(variablesSistema.tipoPersonaJuridico);		
						entidad.setTipoEntidadPubId((solicitudExt.getTipoEntidadId() != null)	? Integer.valueOf(solicitudExt.getTipoEntidadId().toString())	: null);
						entidad.setFlagActualiza(Constantes.FLAG_NO_ACTUALIZO_ENTIDAD);
						entidad.setDescripcionEntidad(beanPersonaJuridica.getPersonaJuridica().getRazonSocial());
						entidad.setUrlWebEntidad((beanPersonaJuridica.getWebs() != null && !beanPersonaJuridica.getWebs().isEmpty())? beanPersonaJuridica.getWebs().get(0).getUrlWeb(): null);
						if(solicitudExt.getUrlLogoEntidad() != null) {
							entidad.setUrlLogoEntidad(solicitudExt.getUrlLogoEntidad());
						}
						entidad.setSigla(solicitudExt.getAbreviatura());
	                	entidad.setNroSindicatos(solicitudExt.getSindicato());								

						entidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
						entidadRepository.save(entidad);
						rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,entidad.getEntidadId().toString());
						//ACTUALIZA ENTIDAD
						LOGGER.debug("Actualiza entidadId a la Solicitud Externa");
						SolicitudExterna solicitudExterna = solicitudExternaRepository.findBySolExtId(solicitudExt.getSolicitudEntidadExtId());
						if ( solicitudExterna != null ) {
							solicitudExterna.setEntidadId(entidad.getEntidadId());
							solicitudExternaRepository.save(solicitudExterna);
						}
						response.setPayload(entidad);
						response.getStatus().setSuccess(Boolean.TRUE);
					}
					
				} else {
					System.out.println("Boolean. false");
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "Se genero una error al actualizar la entidad");
					response.setPayload(null);
				}
			}
			
			
		} catch (Exception e) {
			System.out.println(" Exception :"+e.getMessage());
			return response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		
		return response;
	}
	
	
	public RespBase<Object> searchAndDeleteJefeORHByEntidad (SolicitudExternaDTO solicitudExt, MyJsonWebToken token, Entidad oEntidad) {
		RespBase<Object> response = new RespBase<Object>();
		
		RespBase<RespApiPersona> personaNat = personaApiClient.obtienePersonaPorDocumento(solicitudExt.getTipoDocumento(), solicitudExt.getNumeroDocumento());
		ValidarUsuarioDTO valUsuario = new ValidarUsuarioDTO();
		if (personaNat.getStatus().getSuccess()) {
			List<ValidarUsuarioDTO> lstUsuario = gestionRepository.ObtenerUsuarioExistente(solicitudExt.getNumeroDocumento());
			List<ListaUsuarioRolEntidadDTO> listaRolEntidad = usuarioRepositorySP.obtenerUsuarioPorEntidad(Long.valueOf(variablesSistema.rolJefeOrhGme), oEntidad.getEntidadId());
			if (lstUsuario.size() > 0 ) {
				valUsuario = lstUsuario.get(0);
				if (listaRolEntidad.size() > 0 ) {
					for (int i = 0; i < listaRolEntidad.size(); i++) {
						RespBase<ApiActualizarEstadoRolUsuario> paramSeguridad = new RespBase<ApiActualizarEstadoRolUsuario>();
						ApiActualizarEstadoRolUsuario seguridad = new ApiActualizarEstadoRolUsuario();
						Long usuId = listaRolEntidad.get(i).getUsuarioId();
						if ( valUsuario.getUsuarioId().equals(usuId.intValue()) ) {
							seguridad.setUsuarioRolId(Long.valueOf(variablesSistema.rolJefeOrhGme));
							seguridad.setEstado("1");
							paramSeguridad.setPayload(seguridad);
							RespBase<Object> responseSeg = seguridadApiClient.actualizarEstadoRolUsuario(paramSeguridad.getPayload().getUsuarioRolId(), paramSeguridad);
							
							if( !responseSeg.getStatus().getSuccess()) {
								response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
										"Error desactivando el usuario " + seguridad.getUsuarioRolId() + " con el rol JefeORH");
								return response;
							}
						} else {
							seguridad.setUsuarioRolId(Long.valueOf(variablesSistema.rolJefeOrhGme));
							seguridad.setEstado("0");
							paramSeguridad.setPayload(seguridad);
							RespBase<Object> responseSeg = seguridadApiClient.actualizarEstadoRolUsuario(paramSeguridad.getPayload().getUsuarioRolId(), paramSeguridad);
							
							if( !responseSeg.getStatus().getSuccess()) {
								response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
										"Error desactivando el usuario " + seguridad.getUsuarioRolId() + " con el rol JefeORH");
								return response;
							}
						}
					}
				}
			} else {
				if (listaRolEntidad.size() > 0 ) {
					for (int i = 0; i < listaRolEntidad.size(); i++) {
						RespBase<ApiActualizarEstadoRolUsuario> paramSeguridad = new RespBase<ApiActualizarEstadoRolUsuario>();
						ApiActualizarEstadoRolUsuario seguridad = new ApiActualizarEstadoRolUsuario();
						seguridad.setUsuarioRolId(Long.valueOf(variablesSistema.rolJefeOrhGme));
						seguridad.setEstado("0");
						paramSeguridad.setPayload(seguridad);
						RespBase<Object> responseSeg = seguridadApiClient.actualizarEstadoRolUsuario(paramSeguridad.getPayload().getUsuarioRolId(), paramSeguridad);
						
						if( !responseSeg.getStatus().getSuccess()) {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
									"Error desactivando el usuario " + seguridad.getUsuarioRolId() + " con el rol JefeORH");
							return response;
						}
					}
				}
			}
			response.setPayload(listaRolEntidad);
			response.getStatus().setSuccess(Boolean.TRUE);
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "ERROR AL CONSULTAR PERSONA NATURAL");
		}
		return response;
	}
	
	@SuppressWarnings("unlikely-arg-type")
	public RespBase<Object> searchAndDeleteJefeORHByEntidadEstado (SolicitudExternaDTO solicitudExt, MyJsonWebToken token, Entidad oEntidad) {
		RespBase<Object> response = new RespBase<Object>();
		
		RespBase<RespApiPersona> personaNat = personaApiClient.obtienePersonaPorDocumento(solicitudExt.getTipoDocumento(), solicitudExt.getNumeroDocumento());
		ValidarUsuarioDTO valUsuario = new ValidarUsuarioDTO();
		System.out.println("personaNat:"+personaNat.toString());
		if (personaNat.getStatus().getSuccess()) {
			List<ValidarUsuarioDTO> lstUsuario = gestionRepository.ObtenerUsuarioExistente(solicitudExt.getNumeroDocumento());
			
			
			List<UserRolEntidadDTO> listaRolEntidad = usuarioRepositorySP.obtenerUsuarioPorEntidadSolicitud(Long.valueOf(variablesSistema.rolJefeOrhGme), oEntidad.getEntidadId());
			
			System.out.println("Long.valueOf(variablesSistema.rolJefeOrhGme):"+Long.valueOf(variablesSistema.rolJefeOrhGme));
			System.out.println("oEntidad.getEntidadId():"+oEntidad.getEntidadId());
			
			System.out.println("lstUsuario:"+lstUsuario.toString());
			System.out.println("listaRolEntidad:"+listaRolEntidad.toString());
			LOGGER.debug("lstUsuario :"+lstUsuario.size());
			
			System.out.println("listaRolEntidad.size():"+listaRolEntidad.size());
			
			if (listaRolEntidad.size() > 0 ) {
				for (int i = 0; i < listaRolEntidad.size(); i++) {
					
					System.out.println("usuario rolid:"+listaRolEntidad.get(i).getUsuarioRolId().intValue());
					
					RespBase<ApiActualizarEstadoRolUsuario> paramSeguridad = new RespBase<ApiActualizarEstadoRolUsuario>();
					ApiActualizarEstadoRolUsuario seguridad = new ApiActualizarEstadoRolUsuario();
					seguridad.setUsuarioRolId(listaRolEntidad.get(i).getUsuarioRolId());
					seguridad.setFechaInicioVigencia(ParametrosUtil.fechaHoraActualString());
					seguridad.setFechaFinVigencia(ParametrosUtil.fechaHoraActualString());
					seguridad.setEstado("0");
					paramSeguridad.setPayload(seguridad);
					RespBase<Object> responseSeg = seguridadApiClient.actualizarEstadoRolUsuario(paramSeguridad.getPayload().getUsuarioRolId(), paramSeguridad);
					
					if( !responseSeg.getStatus().getSuccess()) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
								"Error desactivando el usuario " + seguridad.getUsuarioRolId() + " con el rol JefeORH");
						return response;
					}
				}
			}

			// Agregar eliminar Rol JEFE ORH para GDR:
			List<UserRolEntidadDTO> listaRolEntidadGDR = usuarioRepositorySP.obtenerUsuarioPorEntidadSolicitud(Long.valueOf(variablesSistema.rolJefeOrhGdr), oEntidad.getEntidadId());
			
			System.out.println("Long.valueOf(variablesSistema.rolJefeOrhGdr):"+Long.valueOf(variablesSistema.rolJefeOrhGdr));
			System.out.println("listaRolEntidadGDR:"+listaRolEntidadGDR.toString());
			
			System.out.println("listaRolEntidadGDR.size():"+listaRolEntidadGDR.size());
			
			if (listaRolEntidadGDR.size() > 0 ) {
				for (int i = 0; i < listaRolEntidadGDR.size(); i++) {
					
					System.out.println("usuario rolid:"+listaRolEntidadGDR.get(i).getUsuarioRolId().intValue());
					
					RespBase<ApiActualizarEstadoRolUsuario> paramSeguridad = new RespBase<ApiActualizarEstadoRolUsuario>();
					ApiActualizarEstadoRolUsuario seguridad = new ApiActualizarEstadoRolUsuario();
					seguridad.setUsuarioRolId(listaRolEntidadGDR.get(i).getUsuarioRolId());
					seguridad.setFechaInicioVigencia(ParametrosUtil.fechaHoraActualString());
					seguridad.setFechaFinVigencia(ParametrosUtil.fechaHoraActualString());
					seguridad.setEstado("0");
					paramSeguridad.setPayload(seguridad);
					RespBase<Object> responseSeg = seguridadApiClient.actualizarEstadoRolUsuario(paramSeguridad.getPayload().getUsuarioRolId(), paramSeguridad);
					
					if( !responseSeg.getStatus().getSuccess()) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
								"Error desactivando el usuario " + seguridad.getUsuarioRolId() + " con el rol JefeORH en GDR");
						return response;
					}
				}
			}
				
			response.setPayload(listaRolEntidad);
			response.getStatus().setSuccess(Boolean.TRUE);
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "ERROR AL CONSULTAR PERSONA NATURAL");
		}
		return response;
	}
	
	
	@SuppressWarnings("rawtypes")
	private RespBase<Object> registraOrUpdateJefeORH (SolicitudExternaDTO solicitudExt, MyJsonWebToken token, Entidad oEntidad) {
		RespBase<Object> response = new RespBase<>();
		
		try {
			
			PersonaDTO persona = beanAdapterGestores.adapToPersonaJefeORH(solicitudExt);
//			if (solicitudExt.getTipoDocumento().equals(variablesSistema.tipoDocumentoDni)) {
//				persona.setPaisId(variablesSistema.idPaisPeru);
//			} else {
				persona.setPaisId(variablesSistema.idPaisPeru);
//			}
			System.out.println("adapToPersonaJefeORH"+persona.toString());
			
			RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
			ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();
			ParametrosUtil.setearPersonaNatural(apiPersonaNatural, persona);
			personaNaturalResquest.setPayload(apiPersonaNatural);
			
			System.out.println("personaNaturalResquest:"+personaNaturalResquest.toString());
			
			RespBase<Object> responseWS = personaService.obtenerInsertarPersona(variablesSistema.tipoPersonaNatural, 
														persona.getTipoDocumento(),	persona.getNumeroDocumento(), personaNaturalResquest);
			if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						responseWS.getStatus().getError().getMessages().get(0).toString());
				return response;
			}
			
			System.out.println("paseeeeeeeeeeee:");
			
			RespApiPersona personaResponse = (RespApiPersona) responseWS.getPayload();
			String nroDocumento = personaResponse.getDocumentos().get(0).getNumeroDocumento();
			RespBase<RespApiObtenerUsuario> rptaUsuario = seguridadApiClient.buscarUsuariosByFiltroExacto(nroDocumento, null, null, Constantes.ESTADO_ACTIVO);
			if (Boolean.FALSE.equals(rptaUsuario.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						rptaUsuario.getStatus().getError().toString());
				return response;
			}
			System.out.println("paseeeeeeeeeeee 222222222:");
			RespBase<RespGestores> gestorJefeORH = new RespBase<RespGestores>();
			gestorJefeORH = beanAdapterGestores.adapToUsuariosJefeORH(rptaUsuario,nroDocumento,personaResponse,solicitudExt,oEntidad);		
			response.setPayload(gestorJefeORH); 
			System.out.println("paseeeeeeeeeeee 33333333333:");
			if (!usuarioRepositorySP.existeRolUsuario(gestorJefeORH.getPayload().getGestores().getUsuarioId(),Long.valueOf(variablesSistema.rolJefeOrhGme))) {
				RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
				AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
				asignaRolDTO.setRolId(Long.valueOf(variablesSistema.rolJefeOrhGme));
				asignaRolDTO.setEntidadId(oEntidad.getEntidadId());
				LOGGER.debug("id entidad :"+oEntidad.getEntidadId());
				asignaRolDTO.setUsuarioId(gestorJefeORH.getPayload().getGestores().getUsuarioId());
				requestAsignaRol.setPayload(asignaRolDTO);
				RespBase<Object> responseSeguridad = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
				if (Boolean.FALSE.equals(responseSeguridad.getStatus().getSuccess())) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							responseSeguridad.getStatus().getError().toString());
					return response;
				}
			}
			// Agregar Rol JEFE ORH para GDR:
			if (!usuarioRepositorySP.existeRolUsuario(gestorJefeORH.getPayload().getGestores().getUsuarioId(),Long.valueOf(variablesSistema.rolJefeOrhGdr))) {
				RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
				AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
				asignaRolDTO.setRolId(Long.valueOf(variablesSistema.rolJefeOrhGdr));
				asignaRolDTO.setEntidadId(oEntidad.getEntidadId());
				LOGGER.debug("id entidad :"+oEntidad.getEntidadId());
				asignaRolDTO.setUsuarioId(gestorJefeORH.getPayload().getGestores().getUsuarioId());
				requestAsignaRol.setPayload(asignaRolDTO);
				RespBase<Object> responseSeguridad = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
				if (Boolean.FALSE.equals(responseSeguridad.getStatus().getSuccess())) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							responseSeguridad.getStatus().getError().toString());
					return response;
				}
			}
			RespGestores respGestor = new RespGestores();
			Gestores gestor = new Gestores();
			gestor.setEntidadId(oEntidad.getEntidadId());
			gestor.setRolId(Long.valueOf(variablesSistema.rolJefeOrhGme));
			gestor.setPersonaId(gestorJefeORH.getPayload().getGestores().getPersonaId());
			gestor.setUsuarioId(gestorJefeORH.getPayload().getGestores().getUsuarioId());
			gestor.setCorreoId(gestorJefeORH.getPayload().getGestores().getCorreoId());
			gestor.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			gestor=gestoresRepository.save(gestor);
			respGestor.setGestores(gestor);
			response.setPayload(respGestor);
			response.getStatus().setSuccess(Boolean.TRUE);
			System.out.println("paseeeeeeeeeeee 44444444:");
		} catch (Exception e) {
			System.out.println("error :"+e.getMessage());
			// TODO: handle exception
		}
		
		
		return response;
	}
	
	@Override
	public RespBase<Object> rechazarSolicitudExterna(MyJsonWebToken token, Long solicitudExtId) {
		RespBase<Object> response = new RespBase<>();
		try {
			LOGGER.debug("Rechaza Solicitud Externa");
			SolicitudExterna solicitudExt = solicitudExternaRepository.findBySolExtId(solicitudExtId);
			if (solicitudExt  != null) {
				if( solicitudExt.getEstadoSolicitud() == 1) {
					solicitudExt.setEstadoSolicitud(Constantes.SOLICITUD_EXTERNA_RECHAZADO_ID);
					solicitudExt.setSolicitudObs(Constantes.MSG_SOLICITUD_RECHAZADA);
					LOGGER.debug("Guarda Solicitud Externa");
					solicitudExternaRepository.save(solicitudExt);
					
					LOGGER.debug("Envia Correo Solicitud Externa Rechazada");
					if (solicitudExt.getCorreoElectronico() != null ) {
						ReqEmail setarMail = new ReqEmail();
						setarMail = beanAdapterSolicitudExt.adapToDatoCorreoSolExtRechazado(solicitudExt, solicitudExt.getCorreoElectronico());
						ReqBase<ReqEmail> requestMail = new ReqBase<>();
						requestMail.setPayload(setarMail);

						this.notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
					}
					
					if (solicitudExt.getCorreoGestorGdr() != null ) {
						ReqEmail setarMail = new ReqEmail();
						setarMail = beanAdapterSolicitudExt.adapToDatoCorreoSolExtRechazado(solicitudExt, solicitudExt.getCorreoGestorGdr());
						ReqBase<ReqEmail> requestMail = new ReqBase<>();
						requestMail.setPayload(setarMail);

						this.notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
					}
										
					response.setPayload(solicitudExt);
					response.getStatus().setSuccess(Boolean.TRUE);
					
				} else {
					solicitudExt = null;
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR - LA SOLICITUD NO SE ENCUENTRA PENDIENTE");
					response.setPayload(solicitudExt);
				}
			} else {
				solicitudExt = null;
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR - NO SE ENCONTRO SOLICITUD EXTERNA");
				response.setPayload(solicitudExt);
			}
		} catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;
	}

	@Override
	public RespBase<Object> observarSolicitudExterna(MyJsonWebToken token, Long solicitudExtId,
			ReqBase<ReqObservaSolicitudExt> request) {
		
		RespBase<Object> response = new RespBase<>();
		try {
			LOGGER.debug("Observa Solicitud Externa");
			SolicitudExterna solicitudExt = solicitudExternaRepository.findBySolExtId(solicitudExtId);
			if (solicitudExt  != null) {
				if( solicitudExt.getEstadoSolicitud() == 1) {
					solicitudExt.setEstadoSolicitud(Constantes.SOLICITUD_EXTERNA_OBSERVADO_ID);
					solicitudExt.setSolicitudObs(request.getPayload().getSolicitudObs());
					LOGGER.debug("Guarda Solicitud Externa");
					solicitudExternaRepository.save(solicitudExt);
					
					LOGGER.debug("Envia Correo Solicitud Externa Observada");
					if (solicitudExt.getCorreoElectronico() != null ) {
						ReqEmail setarMail = new ReqEmail();
						setarMail = beanAdapterSolicitudExt.adapToDatoCorreoSolExtObservado(solicitudExt, solicitudExt.getCorreoElectronico());
						ReqBase<ReqEmail> requestMail = new ReqBase<>();
						requestMail.setPayload(setarMail);

						this.notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
					}
					
					if (solicitudExt.getCorreoGestorGdr() != null ) {
						ReqEmail setarMail = new ReqEmail();
						setarMail = beanAdapterSolicitudExt.adapToDatoCorreoSolExtObservado(solicitudExt, solicitudExt.getCorreoGestorGdr());
						ReqBase<ReqEmail> requestMail = new ReqBase<>();
						requestMail.setPayload(setarMail);

						this.notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
					}
					
					response.setPayload(solicitudExt);
					response.getStatus().setSuccess(Boolean.TRUE);
					
				} else {
					solicitudExt = null;
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR - LA SOLICITUD NO SE ENCUENTRA PENDIENTE");
					response.setPayload(solicitudExt);
				}
			} else {
				solicitudExt = null;
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"ERROR - NO SE ENCONTRO SOLICITUD EXTERNA");
				response.setPayload(solicitudExt);
			}
		} catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;
	}

	@SneakyThrows
	@Override
	public RespBase<RespRespSubirArchivoNgnx> guardarArchivosNgnx(MultipartFile file, Map<String, String> request) {

		RespRespSubirArchivoNgnx responsePayload = new RespRespSubirArchivoNgnx();
		RespBase<RespRespSubirArchivoNgnx> response = new RespBase<>();

		Parametro parametro = generalRepositorySP.buscarParametro(null, null,Constantes.RUTA_FILE_SERVER_ENTIDAD);
		String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);

		//obtener el dia actual:
		LocalDate fechaActual = LocalDate.now();
		String diaActual = fechaActual.format(DateTimeFormatter.ofPattern("dd"));

		rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,diaActual);
		if (parametro != null) {
			RespBase<ApiUploadFile> requestApiUploadFile = new RespBase<>();
			try{
				ApiUploadFile uploadFile = new ApiUploadFile();
				uploadFile.setExtension("." + ParametrosUtil.extension(file.getOriginalFilename()));
				uploadFile.setFileBase64(Base64.getEncoder().withoutPadding().encodeToString(file.getBytes()));
				uploadFile.setFileName(ParametrosUtil.onlyName(file.getOriginalFilename()));
				uploadFile.setPath(rutaFileServer + "/");
				requestApiUploadFile.setPayload(uploadFile);
				requestApiUploadFile.setPayload(uploadFile);
				RespBase<RespUploadFile> responseWS = maestraApiClient.uploadFile(requestApiUploadFile);

				if (Boolean.TRUE.equals(responseWS.getStatus().getSuccess())) {
					response.getStatus().setSuccess(Boolean.TRUE);
					responsePayload.setCodigoRespuesta(0L);
					//generar uuid automaticamente:
					UUID uuid = UUID.randomUUID();
					String uuidString = uuid.toString();

					responsePayload.setUuid(uuidString);
					responsePayload.setRutaArchivo(uploadFile.getPath() + uploadFile.getFileName() + uploadFile.getExtension());
					responsePayload.setMensajeRespuesta("Operacion Exitosa.");
					response.setPayload(responsePayload);
					return response;
				} else {
					response.getStatus().setSuccess(Boolean.FALSE);
					responsePayload.setCodigoRespuesta(3L);
					responsePayload.setMensajeRespuesta("No se pudo subir el archivo.");
					response.setPayload(responsePayload);
					return response;
				}

			}catch (Exception e) {
				System.out.println("e.getMessage() = " + e.getMessage());
			}

		} else {
			response.getStatus().setSuccess(Boolean.FALSE);
			responsePayload.setCodigoRespuesta(1L);
			responsePayload.setMensajeRespuesta("No se obtuvieron los datos de Maestra.");
		}

		response.setPayload(responsePayload);

		return response;



	}

	@Override
	public RespBase<RespRegistrarSolicitudExterna> actualizarSolicitudExterna(MyJsonWebToken jwt, ReqBase<ReqRegistrarSolicitudExterna> request) {
		RespBase<RespRegistrarSolicitudExterna> response = new RespBase<>();
		RespRegistrarSolicitudExterna responsePayload = new RespRegistrarSolicitudExterna();

		ReqRegistrarSolicitudExterna requestDto = request.getPayload();
		 SolicitudExterna solicitudExt = solicitudExternaRepository.findBySolExtId(requestDto.getSolicitudExtId());
		ValidaGestor validaGestor = this.validarGestor(requestDto);

		RespBase<RespApiFile> subirImagenLogo = null;

		String rutaImage = null;

		if (validaGestor.getCodigoRespuesta().equals(0L)) {

			
			if (Strings.isNotEmpty(requestDto.getBase64Image())) {
				subirImagenLogo = this.maestraApiClient.insertImagen(this.setearDatosImagen(requestDto));
				rutaImage = Boolean.TRUE.equals(subirImagenLogo.getStatus().getSuccess()) &&
						    Strings.isNotEmpty(subirImagenLogo.getPayload().getPathRelative()) ? subirImagenLogo.getPayload().getPathRelative() : "";
			}else {
				rutaImage = solicitudExt.getUrlLogoEntidad();
			}

			SolicitudExterna solicitudExterna =
					this.setearSolicitudExterna(requestDto, rutaImage, jwt);
			solicitudExterna.setSolicitudEntidadExtId(requestDto.getSolicitudExtId());
			this.solicitudExternaRepository.save(solicitudExterna);

			ReqEmail setarMail = this.setarDatoCorreoRegSolExt(requestDto);
			ReqBase<ReqEmail> requestMail = new ReqBase<>();
			requestMail.setPayload(setarMail);

			this.notificacionApiClient.enviarCorreo(requestMail);

			responsePayload.setSolicitudEntidadExtId(solicitudExterna.getSolicitudEntidadExtId());
			responsePayload.setCodigoRespuesta(0L);
			responsePayload.setMensajeRespuesta("Operacion Exitosa");
			response.setPayload(responsePayload);
			response.getStatus().setSuccess(Boolean.TRUE);

		} else {

			if (validaGestor.getCodigoRespuesta() < 0) {
				RespBase.Status.Error error = new RespBase.Status.Error();
				List<String> msjError = new ArrayList<>();
				msjError.add(validaGestor.getMensajeRespuesta());
				error.setMessages(msjError);
				response.getStatus().setError(error);
				response.getStatus().setSuccess(Boolean.FALSE);
			}

			if (validaGestor.getCodigoRespuesta() > 0) {
				responsePayload.setCodigoRespuesta(validaGestor.getCodigoRespuesta());
				responsePayload.setMensajeRespuesta(validaGestor.getMensajeRespuesta());

				response.setPayload(responsePayload);
				response.getStatus().setSuccess(Boolean.FALSE);
			}


		}

		return response;
	}
	
	@Override
	public RespBase<Encrypt> getEncryptSolicitudExternaId(Long solicitudExternaId) {
		LOGGER.info("Metodo getEncryptSolicitudExternaId...");
		RespBase<Encrypt> response = new RespBase<Encrypt>();
		Encrypt responsePayload = new Encrypt();
		try {
			responsePayload.setEncrypt(variablesSistema.linkSGMSolicitudEditar + "?id=" + EncryptUtil.encrypt(String.valueOf(solicitudExternaId), variablesSistema.getSecretKey().getBytes(StandardCharsets.UTF_8)));
			response.setPayload(responsePayload);
			response.getStatus().setSuccess(true);
		} catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;
	}



}