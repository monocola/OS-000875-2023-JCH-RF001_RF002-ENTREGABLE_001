package pe.gob.servir.entidad.adapter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.RandomStringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.api.dto.ApiSeguridadRequestDTO;
import pe.gob.servir.entidad.api.dto.DataEmail;
import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.api.dto.ReqEmail;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.NotificacionApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.Entidad;
import pe.gob.servir.entidad.model.GestorDTO;
import pe.gob.servir.entidad.model.Gestores;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.request.ReqActualizaGestorORH;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqGestorORH;
import pe.gob.servir.entidad.response.RespApiObtenerUsuario;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespApiSeguridad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespGestores;
import pe.gob.servir.entidad.util.FilesUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Component
public class BeanAdapterGestores {
	
	@Autowired
	private SeguridadApiClient seguridadApiClient;
	
	@Autowired
	private NotificacionApiClient notificacionApiClient;
	
	@Autowired
	VariablesSistema variablesSistema;
	
	public PersonaDTO adapToPersonaDTO(ReqGestorORH request) {
		PersonaDTO personaResponsable = new PersonaDTO();

		personaResponsable.setTipoDocumento(request.getTipoDocumento().intValue());
		personaResponsable.setNumeroDocumento(request.getNumeroDocumento());
		personaResponsable.setNombres(request.getNombres());
		personaResponsable.setApellidoPaterno(request.getApellidoPaterno());
		personaResponsable.setApellidoMaterno(request.getApellidoMaterno());
		personaResponsable.setCorreoPrincipalId(null);
		personaResponsable.setCorreoPrincipal(request.getCorreo());
		personaResponsable.setTelefonoFijoId(null);
		personaResponsable.setTelefonoFijo(request.getTelefono());
		personaResponsable.setFechaNacimiento(request.getFechaNacimiento());

		return personaResponsable;
	}

	public RespBase<RespGestores> adapToUsuarios(RespBase<RespApiObtenerUsuario> rptaUsuario, String nroDocumento,
			RespApiPersona personaResponse, ReqGestorORH request) {
		Long usuarioId = null;
		RespBase<RespGestores> response = new RespBase<>();
		RespGestores respGestor = new RespGestores();
		Gestores gestor = new Gestores();
		ReqBase<ReqEmail> requestMail = new ReqBase<>();
		
		if (rptaUsuario.getPayload().getCount() == 0 || rptaUsuario.getPayload().getItems() == null	|| rptaUsuario.getPayload().getItems().isEmpty()) {
			RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
			String password = null;
			ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(nroDocumento,request.getCorreo(),personaResponse.getPersona().getPersonaId(), 
			Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, password);
			apiUsuario.setPayload(requestCreaUsuario);
			RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(request.getEntidadId(), apiUsuario);
			if (Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						respuestaCreaUser.getStatus().getError().toString());
				return response;
			} else {
				usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
			}
			requestMail = adapToCorreoUsuarioNuevo(respuestaCreaUser,personaResponse,request);
			notificacionApiClient.enviarCorreoCredenciales(requestMail);
		} else {
			usuarioId = rptaUsuario.getPayload().getItems().get(0).getUsuarioId();
			// COMPROBAMOS QUE pertenesca a esa entidad
			RespBase<RespApiObtenerUsuario> respuestaWS = seguridadApiClient.buscarUsuarioEntidad(request.getEntidadId());
			if (Boolean.FALSE.equals(respuestaWS.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						respuestaWS.getStatus().getError().toString());
				return response;
			}
			boolean encontro = false;
			if (respuestaWS.getPayload().getItems() != null	&& !respuestaWS.getPayload().getItems().isEmpty()) {
				for (RespApiSeguridad itemSeg : respuestaWS.getPayload().getItems()) {
					if (itemSeg.getUsuarioId().longValue() == usuarioId.longValue()) {
						encontro = true;
						break;
					}
				}
			}
			if (!encontro) {
				RespBase<Object> responseSeguridad = seguridadApiClient.asignarUsuarioEntidad(request.getEntidadId(),usuarioId, EstadoRegistro.ACTIVO.getCodigo());
				if (Boolean.FALSE.equals(responseSeguridad.getStatus().getSuccess())) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							responseSeguridad.getStatus().getError().toString());
					return response;
				}
			}
			requestMail = adapToCorreoUsuarioExistente(rptaUsuario,personaResponse,request);
//			enviarCorreoExisteUsuario(personaResponse.getCorreos().get(0).getCorreo(),rptaUsuario.getPayload().getItems().get(0).getCorreoElectronico(),rptaUsuario.getPayload().getItems().get(0).getUsuario(), false);
			notificacionApiClient.enviarCorreoCredenciales(requestMail);
		}
		gestor.setUsuarioId(usuarioId);
		gestor.setPersonaId(personaResponse.getPersona().getPersonaId());
		gestor.setCorreoId(personaResponse.getCorreos().get(0).getCorreoId());
		respGestor.setGestores(gestor);
		response.setPayload(respGestor);
		return response;
	}

	private ReqBase<ReqEmail> adapToCorreoUsuarioExistente(RespBase<RespApiObtenerUsuario> rptaUsuario,
			RespApiPersona personaResponse, ReqGestorORH request) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		System.out.println(" variablesSistema.linkGdr 1:"+ variablesSistema.linkGdr);
		System.out.println(" variablesSistema.correoGdr 1:"+ variablesSistema.correoGdr);
		parametros.put("NOMBRE_USUARIO", personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno());		
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		parametros.put("USUARIO", rptaUsuario.getPayload().getItems().get(0).getUsuario());		
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		dataEmail.setTemplateCode(Constantes.EXI_GESTOR_ORH);
		dataEmail.setSubject(null);
		dataEmail.setTo(request.getCorreo());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
	}

	private ReqBase<ReqEmail> adapToCorreoUsuarioNuevo(RespBase<RespApiSeguridad> respuestaCreaUser,
			RespApiPersona personaResponse, ReqGestorORH request) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		parametros.put("NOMBRE_USUARIO", personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno());
		parametros.put("LINK_SGM", variablesSistema.linkSGM);
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		System.out.println(" variablesSistema.linkGdr 1+2:"+ variablesSistema.linkGdr);
		System.out.println(" variablesSistema.correoGdr 12:"+ variablesSistema.correoGdr);
		System.out.println("password:"+ respuestaCreaUser.getPayload().getPasswordTemporal());
		parametros.put("USUARIO", respuestaCreaUser.getPayload().getUsuario());
		parametros.put("CLAVE", respuestaCreaUser.getPayload().getPasswordTemporal());
		System.out.println("password 444:"+ respuestaCreaUser.getPayload().getPasswordTemporal());
		
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		dataEmail.setTemplateCode(Constantes.ASG_GESTOR_ORH);
		dataEmail.setSubject(null);
		dataEmail.setTo(request.getCorreo());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
		
	}

	public GestorDTO adapToListaGestorDTO(List<GestorDTO> gestor) {
		GestorDTO gestorDTO = new GestorDTO();
		if (gestor.size() > 0) {
			gestorDTO.setGestorId(gestor.get(0).getGestorId());
			gestorDTO.setEntidadId(gestor.get(0).getEntidadId());
			gestorDTO.setPersonaId(gestor.get(0).getPersonaId());
			gestorDTO.setTipoDocumento(gestor.get(0).getTipoDocumento());
			gestorDTO.setNumeroDocumento(gestor.get(0).getNumeroDocumento());
			gestorDTO.setNombres(gestor.get(0).getNombres());
			gestorDTO.setApellidoPaterno(gestor.get(0).getApellidoPaterno());
			gestorDTO.setApellidoMaterno(gestor.get(0).getApellidoMaterno());
			gestorDTO.setNombreCompleto(gestor.get(0).getNombreCompleto());
			gestorDTO.setCorreo(gestor.get(0).getCorreo());
			gestorDTO.setCorreoId(gestor.get(0).getCorreoId());
			gestorDTO.setNumeroTelefono(gestor.get(0).getNumeroTelefono());
			gestorDTO.setTelefonoId(gestor.get(0).getTelefonoId());
			gestorDTO.setTipoTelefono(gestor.get(0).getTipoTelefono());
			gestorDTO.setAnexoTelefono(gestor.get(0).getAnexoTelefono());
			gestorDTO.setFechaNacimiento(gestor.get(0).getFechaNacimiento());
			gestorDTO.setUsuarioId(gestor.get(0).getUsuarioId());
			gestorDTO.setRolId(gestor.get(0).getRolId());
			gestorDTO.setEstadoRegistro(gestor.get(0).getEstadoRegistro());
			gestorDTO.setEstado(gestor.get(0).getEstado());
			gestorDTO.setNumeroCelular(gestor.get(0).getNumeroCelular());
			gestorDTO.setCelularId(gestor.get(0).getCelularId());
			gestorDTO.setTipoCelular(gestor.get(0).getTipoCelular());
		}
		
		return gestorDTO;
	}
	
	public PersonaDTO adapToUPDPersonaDTO(ReqActualizaGestorORH request) {
		PersonaDTO personaResponsable = new PersonaDTO();

		personaResponsable.setTipoDocumento(request.getTipoDocumento().intValue());
		personaResponsable.setNumeroDocumento(request.getNumeroDocumento());
		personaResponsable.setCorreoPrincipalId(request.getCorreoId());
		personaResponsable.setCorreoPrincipal(request.getCorreo());
		personaResponsable.setTelefonoFijoId(request.getTelefonoId());
		personaResponsable.setTelefonoFijo(request.getTelefono());
		personaResponsable.setCelularPrincipal(request.getCelular());
		personaResponsable.setCelularLaboral(request.getAnexo());
		
		return personaResponsable;
	}

	public PersonaDTO adapToPersonaJefeORH(SolicitudExternaDTO solicitudExt) {
		PersonaDTO personaResponsable = new PersonaDTO();

		personaResponsable.setTipoDocumento(solicitudExt.getTipoDocumento().intValue());
		personaResponsable.setNumeroDocumento(solicitudExt.getNumeroDocumento());
		personaResponsable.setNombres(solicitudExt.getNombres().trim());
		personaResponsable.setApellidoPaterno(solicitudExt.getApellidoPaterno().trim());
		personaResponsable.setApellidoMaterno(solicitudExt.getApellidoMaterno().trim());
		personaResponsable.setCorreoPrincipalId(null);
		personaResponsable.setCorreoPrincipal(solicitudExt.getCorreoElectronico().trim());
		personaResponsable.setTelefonoFijoId(null);
		personaResponsable.setTelefonoFijo(solicitudExt.getTelefonoFijo());
		personaResponsable.setCelularPrincipalId(null);
		personaResponsable.setCelularPrincipal(solicitudExt.getCelular());
		personaResponsable.setFechaNacimiento(solicitudExt.getFechaNacimiento());
		
		return personaResponsable;
	}
	
	public RespBase<RespGestores> adapToUsuariosJefeORH(RespBase<RespApiObtenerUsuario> rptaUsuario, String nroDocumento,
			RespApiPersona personaResponse, SolicitudExternaDTO solicitudExt, Entidad oEntidad) {
		Long usuarioId = null;
		RespBase<RespGestores> response = new RespBase<>();
		RespGestores respGestor = new RespGestores();
		Gestores gestor = new Gestores();
		ReqBase<ReqEmail> requestMail = new ReqBase<>();
		ReqBase<ReqEmail> requestMailGestor = new ReqBase<>();
		String password = RandomStringUtils.randomAlphanumeric(8);
		System.out.println("paseeeeeeeeeeee bbbbb:");
		
		if (rptaUsuario.getPayload().getCount() == 0 || rptaUsuario.getPayload().getItems() == null	|| rptaUsuario.getPayload().getItems().isEmpty()) {
			RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
			ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(nroDocumento,solicitudExt.getCorreoElectronico(),personaResponse.getPersona().getPersonaId(),
					Constantes.ASG_JEFE_ORH,password);
			apiUsuario.setPayload(requestCreaUsuario);
			
			System.out.println("password :"+password);
			System.out.println("requestCreaUsuario :"+requestCreaUsuario.toString());
			String nameUsuario = personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno();
			RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidadSolExt(oEntidad.getEntidadId(), apiUsuario, nameUsuario);
			if (Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						respuestaCreaUser.getStatus().getError().toString());
				return response;
			} else {
				usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
			}
			 
			requestMail = adapToCorreoUsuarioNuevoJefeORH(respuestaCreaUser,personaResponse,solicitudExt,password);
			notificacionApiClient.enviarCorreoSolicitudExt(requestMail);

			if (solicitudExt.getCorreoGestorGdr() != null ) {
			
				requestMailGestor =  adapToCorreoUsuarioNuevoJefeORHGestor(respuestaCreaUser,personaResponse,solicitudExt,password);
				notificacionApiClient.enviarCorreoSolicitudExt(requestMailGestor);
			}
		
		} else { 
			
			System.out.println("paseeeeeeeeeeee nnnnnnnnnnnnnnnnnnnn:");
			usuarioId = rptaUsuario.getPayload().getItems().get(0).getUsuarioId();
			// COMPROBAMOS QUE pertenesca a esa entidad
			RespBase<RespApiObtenerUsuario> respuestaWS = seguridadApiClient.buscarUsuarioEntidadSolicitudExt(oEntidad.getEntidadId());
			if (Boolean.FALSE.equals(respuestaWS.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						respuestaWS.getStatus().getError().toString());
				return response;
			}
			boolean encontro = false;
			if (respuestaWS.getPayload().getItems() != null	&& !respuestaWS.getPayload().getItems().isEmpty()) {
				for (RespApiSeguridad itemSeg : respuestaWS.getPayload().getItems()) {
					if (itemSeg.getUsuarioId().longValue() == usuarioId.longValue()) {
						encontro = true;
						break;
					}
				}
			}
			if (!encontro) {
				RespBase<Object> responseSeguridad = seguridadApiClient.asignarUsuarioEntidad(oEntidad.getEntidadId(),usuarioId, EstadoRegistro.ACTIVO.getCodigo());
				if (Boolean.FALSE.equals(responseSeguridad.getStatus().getSuccess())) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							responseSeguridad.getStatus().getError().toString());
					return response;
				}
			} 
			requestMail = adapToCorreoUsuarioExistenteJefeORHGME(rptaUsuario,personaResponse,solicitudExt,password);
//			enviarCorreoExisteUsuario(personaResponse.getCorreos().get(0).getCorreo(),rptaUsuario.getPayload().getItems().get(0).getCorreoElectronico(),rptaUsuario.getPayload().getItems().get(0).getUsuario(), false);
			notificacionApiClient.enviarCorreoSolicitudExt(requestMail);
			if (solicitudExt.getCorreoGestorGdr() != null ) {
				System.out.println("paseeeeeeeeeeee getCorreoGestorGdr:");
				
				requestMailGestor = adapToCorreoUsuarioExistenteJefeORHGestor(rptaUsuario,personaResponse,solicitudExt);
				notificacionApiClient.enviarCorreoSolicitudExt(requestMailGestor);
			}
		}
		gestor.setUsuarioId(usuarioId);
		gestor.setPersonaId(personaResponse.getPersona().getPersonaId());
		gestor.setCorreoId(personaResponse.getCorreos().get(0).getCorreoId());
		respGestor.setGestores(gestor);
		response.setPayload(respGestor);
		return response;
	}
	
	private ReqBase<ReqEmail> adapToCorreoUsuarioNuevoJefeORH(RespBase<RespApiSeguridad> respuestaCreaUser,
			RespApiPersona personaResponse, SolicitudExternaDTO solicitudExt, String password) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		parametros.put("NOMBRE_USUARIO", personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno());
		parametros.put("LINK_SGM", variablesSistema.linkSGM);
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		parametros.put("USUARIO", respuestaCreaUser.getPayload().getUsuario());
		parametros.put("CLAVE", password);
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		System.out.println("password:"+ password);
		System.out.println(" variablesSistema.linkGdr 3:"+ variablesSistema.linkGdr);
		System.out.println(" variablesSistema.linkGdr 3:"+ variablesSistema.linkGdr);
		System.out.println("password:"+ password);
		dataEmail.setTemplateCode(Constantes.ASG_JEFE_ORH);
		dataEmail.setSubject(null);
		dataEmail.setTo(solicitudExt.getCorreoElectronico());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
		
	}
	
	private ReqBase<ReqEmail> adapToCorreoUsuarioNuevoJefeORHGestor(RespBase<RespApiSeguridad> respuestaCreaUser,
			RespApiPersona personaResponse, SolicitudExternaDTO solicitudExt, String password) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		parametros.put("NOMBRE_USUARIO", personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno());
		parametros.put("LINK_SGM", variablesSistema.linkSGM);
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		parametros.put("USUARIO", respuestaCreaUser.getPayload().getUsuario());
		parametros.put("CLAVE", password);
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		System.out.println("password:"+ password);
		System.out.println(" variablesSistema.linkGdr 44:"+ variablesSistema.linkGdr);
		System.out.println(" variablesSistema.correoGdr 44:"+ variablesSistema.correoGdr);
		dataEmail.setTemplateCode(Constantes.ASG_JEFE_ORH);
		dataEmail.setSubject(null);
		dataEmail.setTo(solicitudExt.getCorreoGestorGdr());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
		
	}
	
	private ReqBase<ReqEmail> adapToCorreoUsuarioExistenteJefeORH(RespBase<RespApiObtenerUsuario> rptaUsuario,
			RespApiPersona personaResponse, SolicitudExternaDTO solicitudExt) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		System.out.println("paseeeeeeeeeeee adapToCorreoUsuarioExistenteJefeORH:");
		parametros.put("NOMBRE_USUARIO", personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno());		
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		parametros.put("USUARIO", rptaUsuario.getPayload().getItems().get(0).getUsuario());		
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		dataEmail.setTemplateCode(Constantes.EXI_JEFE_ORH);
		dataEmail.setSubject(null);
		System.out.println("parametros:"+parametros.toString());
		dataEmail.setTo(solicitudExt.getCorreoElectronico());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
	}
	
	private ReqBase<ReqEmail> adapToCorreoUsuarioExistenteJefeORHGME(RespBase<RespApiObtenerUsuario> rptaUsuario,
			RespApiPersona personaResponse, SolicitudExternaDTO solicitudExt,String password) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		System.out.println("paseeeeeeeeeeee adapToCorreoUsuarioExistenteJefeORH:");
		parametros.put("NRO_DOCUMENTO", personaResponse.getDocumentos().get(0).getNumeroDocumento());		
		parametros.put("LINK_SGM", variablesSistema.linkSGM);
		parametros.put("USUARIO", rptaUsuario.getPayload().getItems().get(0).getUsuario());		
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		parametros.put("CORREO_ELECTRONICO", personaResponse.getCorreos().get(0).getCorreo());
		dataEmail.setTemplateCode(Constantes.PLANTILLA_USUARIO_EXISTENTE);
		dataEmail.setSubject(null);
		System.out.println("parametros:"+parametros.toString());
		dataEmail.setTo(solicitudExt.getCorreoElectronico());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
	}
	
	private ReqBase<ReqEmail> adapToCorreoUsuarioExistenteJefeORHGestor(RespBase<RespApiObtenerUsuario> rptaUsuario,
			RespApiPersona personaResponse, SolicitudExternaDTO solicitudExt) {
		ReqBase<ReqEmail> sendEmail =  new ReqBase<>();
		ReqEmail email =  new ReqEmail();
		DataEmail dataEmail  = new DataEmail();
		Map<String, Object> parametros =  new HashMap<>();
		System.out.println(" variablesSistema.linkGdr 1:"+ variablesSistema.linkGdr);
		System.out.println(" variablesSistema.correoGdr 1:"+ variablesSistema.correoGdr);
		parametros.put("NOMBRE_USUARIO", personaResponse.getPersonaNatural().getNombres() + " " + personaResponse.getPersonaNatural().getApellidoPaterno() + " " + personaResponse.getPersonaNatural().getApellidoMaterno());		
		parametros.put("LINK_GDR", variablesSistema.linkGdr);
		parametros.put("USUARIO", rptaUsuario.getPayload().getItems().get(0).getUsuario());		
		parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
		dataEmail.setTemplateCode(Constantes.EXI_JEFE_ORH);
		dataEmail.setSubject(null);
		dataEmail.setTo(solicitudExt.getCorreoGestorGdr());
		dataEmail.setBodyValues(parametros);
		email.setData(dataEmail);
		email.setIncludeAttachments(false);
		sendEmail.setPayload(email);
		return sendEmail;
	}

}
