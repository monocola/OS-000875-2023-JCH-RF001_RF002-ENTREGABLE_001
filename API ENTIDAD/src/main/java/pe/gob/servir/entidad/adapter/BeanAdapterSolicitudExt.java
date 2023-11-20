package pe.gob.servir.entidad.adapter;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.api.dto.DataEmail;
import pe.gob.servir.entidad.api.dto.ReqEmail;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.model.EntidadTipoRucDTO;
import pe.gob.servir.entidad.model.SolicitudExterna;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespSolicitudExt;
import pe.gob.servir.entidad.util.EncryptUtil;

@Component
public class BeanAdapterSolicitudExt {

	@Autowired
	private VariablesSistema variablesSistema;

	public RespSolicitudExt adapToRespBaseSolicitudExt(RespBase<RespApiPersona> respPersona) {
		RespSolicitudExt solicitud = new RespSolicitudExt();
		if ( respPersona.getPayload().getPersonaJuridica() != null ) {
			solicitud.setPersonaId(respPersona.getPayload().getPersonaJuridica().getPersonaId());
			solicitud.setRazonSocial(respPersona.getPayload().getPersonaJuridica().getRazonSocial());
			solicitud.setNombreEntidad(respPersona.getPayload().getPersonaJuridica().getNombreComercial());
			
			solicitud.setRucEntidad(respPersona.getPayload().getDocumentos().get(0).getNumeroDocumento());
			solicitud.setDocumentoId(respPersona.getPayload().getDocumentos().get(0).getDocumentoId());
			solicitud.setTipoDocumento(respPersona.getPayload().getDocumentos().get(0).getTipoDocumento());
			
			//Extras
			solicitud.setActividadEconomicaPrincipal(respPersona.getPayload().getPersonaJuridica().getActividadEconomicaPrincipal());
			solicitud.setEstadoContribuyente(respPersona.getPayload().getPersonaJuridica().getEstadoContribuyente());
			solicitud.setValidadoSunat(respPersona.getPayload().getPersonaJuridica().getValidadoSunat());
			solicitud.setFlagEntidadExite("NO");
			
		}
		
		
		return solicitud;
	}
	
	public RespSolicitudExt adapToApiPersonaSolicitudExt(RespBase<RespApiPersona> respPersona, SolicitudExterna solicitudExt) {
		RespSolicitudExt solicitud = new RespSolicitudExt();
		// Persona
		if(respPersona.getPayload().getPersonaJuridica() != null ) {
			solicitud.setPersonaId(respPersona.getPayload().getPersonaJuridica().getPersonaId());
			solicitud.setRazonSocial(respPersona.getPayload().getPersonaJuridica().getRazonSocial());
			solicitud.setNombreEntidad(respPersona.getPayload().getPersonaJuridica().getNombreComercial());
			solicitud.setRucEntidad(respPersona.getPayload().getDocumentos().get(0).getNumeroDocumento());
			solicitud.setDocumentoId(respPersona.getPayload().getDocumentos().get(0).getDocumentoId());
			solicitud.setTipoDocumento(respPersona.getPayload().getDocumentos().get(0).getTipoDocumento());
			solicitud.setActividadEconomicaPrincipal(respPersona.getPayload().getPersonaJuridica().getActividadEconomicaPrincipal());
			solicitud.setEstadoContribuyente(respPersona.getPayload().getPersonaJuridica().getEstadoContribuyente());
			solicitud.setValidadoSunat(respPersona.getPayload().getPersonaJuridica().getValidadoSunat());
		} else {
			solicitud.setSolicitudEntidadExtId(solicitudExt.getSolicitudEntidadExtId());
			solicitud.setRazonSocial(solicitudExt.getRazonSocial());
			solicitud.setNombreEntidad(solicitudExt.getNombreEntidad());
			solicitud.setRucEntidad(solicitudExt.getRucEntidad());;
		}
		//Flag
		if (respPersona.getPayload().getPersona() == null ) {
			solicitud.setFlagEntidadExite("NO");
		} else {
			solicitud.setFlagEntidadExite("SI");
		}
		
		// Solicitud
		solicitud.setAbreviatura(solicitudExt.getAbreviatura());
		solicitud.setNivelGobId(solicitudExt.getNivelGobiernoId());
		solicitud.setNivelGob(solicitudExt.getNivelGobierno());
		solicitud.setSectorId(solicitudExt.getSectorId());
		solicitud.setSector(solicitudExt.getSector());
		solicitud.setTipoEntidadId(solicitudExt.getTipoEntidadId());
		solicitud.setTipoEntidad(solicitudExt.getTipoEntidad());
		solicitud.setUrlLogoEntidad(solicitudExt.getUrlLogoEntidad());
		solicitud.setFechaNacimiento(solicitudExt.getFechaNacimiento());
		solicitud.setTelefonoFijo(solicitudExt.getTelefonoFijo());
		solicitud.setCelular(solicitudExt.getCelular());
		solicitud.setCorreoElectronico(solicitudExt.getCorreoElectronico());
		solicitud.setUuidDocumento(solicitudExt.getUuidDocumento());
		solicitud.setEstadoSolicitud(solicitudExt.getEstadoSolicitud());
		
		return solicitud;
		
	}
	
	public RespSolicitudExt adapToRespBaseEntidadTipoRucDTO(EntidadTipoRucDTO entidad) {
		RespSolicitudExt solicitud = new RespSolicitudExt();
		
		if (entidad.getEntidadId() == null ) {
			solicitud.setFlagEntidadExite("NO");
		} else {
			solicitud.setFlagEntidadExite("SI");
		}
		
		solicitud.setPersonaId(entidad.getPersonaId());
		solicitud.setEntidadId(entidad.getEntidadId());
		solicitud.setRazonSocial(entidad.getRazonSocial());
		solicitud.setNombreEntidad(entidad.getNombreEntidad());
		solicitud.setAbreviatura(entidad.getAbreviatura());
		solicitud.setTipoDocumento(entidad.getTipoDocumento());
		solicitud.setRucEntidad(entidad.getRucEntidad());
		solicitud.setNivelGobId(entidad.getNivelGobId());
		solicitud.setNivelGob(entidad.getNivelGob());
		solicitud.setSectorId(entidad.getSectorId());
		solicitud.setSector(entidad.getSector());
		solicitud.setTipoEntidadId(entidad.getTipoEntidadId());
		solicitud.setTipoEntidad(entidad.getTipoEntidad());
		solicitud.setUrlLogoEntidad(entidad.getUrlLogoEntidad());
		
		return solicitud;
		
	}

	public SolicitudExternaDTO adapToSolExtDTO(SolicitudExterna solicitud) {
		SolicitudExternaDTO solicitudDTO = new SolicitudExternaDTO();
		solicitudDTO.setSolicitudEntidadExtId(solicitud.getSolicitudEntidadExtId());
		solicitudDTO.setRucEntidad(solicitud.getRucEntidad());
		solicitudDTO.setRazonSocial(solicitud.getRazonSocial());
		solicitudDTO.setNombreEntidad(solicitud.getNombreEntidad());
		solicitudDTO.setTipoDocumento(solicitud.getTipoDocumento());
		solicitudDTO.setNumeroDocumento(solicitud.getNumeroDocumento());
		solicitudDTO.setApellidoPaterno(solicitud.getApellidoPaterno());
		solicitudDTO.setApellidoMaterno(solicitud.getApellidoMaterno());
		solicitudDTO.setNombres(solicitud.getNombres());
		solicitudDTO.setUuidDocumento(solicitud.getUuidDocumento());
		solicitudDTO.setAbreviatura(solicitud.getAbreviatura());
		solicitudDTO.setNivelGobiernoId(solicitud.getNivelGobiernoId());
		solicitudDTO.setNivelGobierno(solicitud.getNivelGobierno());
		solicitudDTO.setSectorId(solicitud.getSectorId());
		solicitudDTO.setSector(solicitud.getSector());
		solicitudDTO.setTipoEntidadId(solicitud.getTipoEntidadId());
		solicitudDTO.setTipoEntidad(solicitud.getTipoEntidad());
		solicitudDTO.setUrlLogoEntidad(solicitud.getUrlLogoEntidad());
		solicitudDTO.setFechaNacimiento(solicitud.getFechaNacimiento());
		solicitudDTO.setTelefonoFijo(solicitud.getTelefonoFijo());
		solicitudDTO.setAnexo(solicitud.getAnexo());
		solicitudDTO.setCelular(solicitud.getCelular());
		solicitudDTO.setCorreoElectronico(solicitud.getCorreoElectronico());
		solicitudDTO.setNombreCompleto(solicitud.getApellidoPaterno() + ' ' + solicitud.getApellidoMaterno() + ' '+ solicitud.getNombres());
		solicitudDTO.setFechaSolicitud(solicitud.getFechaCreacion().toString());
		solicitudDTO.setEstadoId(solicitud.getEstadoSolicitud());
		
		return solicitudDTO;
		
	}
	
	public SolicitudExterna adapToSolicitudExternaDTO( SolicitudExternaDTO solicitud) {
		SolicitudExterna externa = new SolicitudExterna();
		externa.setSolicitudEntidadExtId(solicitud.getSolicitudEntidadExtId());
		externa.setRucEntidad(solicitud.getRucEntidad());
		externa.setRazonSocial(solicitud.getRazonSocial());
		externa.setAbreviatura(solicitud.getAbreviatura());
		externa.setNombreEntidad(solicitud.getNombreEntidad());
		externa.setNivelGobiernoId(solicitud.getNivelGobiernoId());
		externa.setNivelGobierno(solicitud.getNivelGobierno());
		externa.setSectorId(solicitud.getSectorId());
		externa.setSector(solicitud.getSector());
		externa.setTipoEntidadId(solicitud.getTipoEntidadId());
		externa.setTipoEntidad(solicitud.getTipoEntidad());
		externa.setUrlLogoEntidad(solicitud.getUrlLogoEntidad());
		externa.setTipoDocumento(solicitud.getTipoDocumento());
		externa.setNumeroDocumento(solicitud.getNumeroDocumento());
		externa.setApellidoPaterno(solicitud.getApellidoPaterno());
		externa.setApellidoMaterno(solicitud.getApellidoMaterno());
		externa.setNombres(solicitud.getNombres());
		externa.setFechaNacimiento(solicitud.getFechaNacimiento());
		externa.setTelefonoFijo(solicitud.getTelefonoFijo());
		externa.setAnexo(solicitud.getAnexo());
		externa.setCelular(solicitud.getCelular());;
		externa.setCorreoElectronico(solicitud.getCorreoElectronico());
		externa.setUuidDocumento(solicitud.getUuidDocumento());
		externa.setEstadoSolicitud(solicitud.getEstadoId());
		externa.setSolicitudObs(solicitud.getSolicitudObs());
		externa.setCorreoGestorGdr(solicitud.getCorreoGestorGdr());

		return externa;
		
	}

	public ReqEmail adapToDatoCorreoSolExtRechazado(SolicitudExterna request, String correo) {
		ReqEmail email =  new ReqEmail();
		if( correo != null) {
			DataEmail dataEmail  = new DataEmail();
			Map<String, Object> parametros =  new HashMap<>();
			parametros.put("NOMBRE_USUARIO", request.getNombres() + " " + request.getApellidoPaterno() + " " + request.getApellidoMaterno());
			parametros.put("LINK_SGM", variablesSistema.linkSGM);
			parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
			parametros.put("NOMBRE_ENTIDAD", request.getRazonSocial());
			dataEmail.setTemplateCode(Constantes.CAN_SOL_ENT_EXT);
			dataEmail.setSubject(Constantes.SUBJECT_CAN_SOL_ENT_EXT);
			dataEmail.setTo(correo);
			dataEmail.setBodyValues(parametros);
			email.setData(dataEmail);
			email.setIncludeAttachments(false);	
		}
		return email;
	}
	
	public ReqEmail adapToDatoCorreoSolExtObservado(SolicitudExterna request, String correo) throws Exception {
		
		ReqEmail email =  new ReqEmail();
		if( correo != null) {
			DataEmail dataEmail  = new DataEmail();
			Map<String, Object> parametros =  new HashMap<>();
			parametros.put("NOMBRE_USUARIO", request.getNombres() + " " + request.getApellidoPaterno() + " " + request.getApellidoMaterno());
			parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
			parametros.put("OBSERVACION", request.getSolicitudObs());
			parametros.put("LINK_SGM", variablesSistema.linkSGMSolicitudEditar + "?id=" + EncryptUtil.encrypt(String.valueOf(request.getSolicitudEntidadExtId()), variablesSistema.getSecretKey().getBytes(StandardCharsets.UTF_8)));
			parametros.put("NOMBRE_ENTIDAD", request.getRazonSocial());
			dataEmail.setTemplateCode(Constantes.OBS_SOL_ENT_EXT);
			dataEmail.setSubject(Constantes.SUBJECT_OBS_SOL_ENT_EXT);
			dataEmail.setTo(correo);
			dataEmail.setBodyValues(parametros);
			email.setData(dataEmail);
			email.setIncludeAttachments(false);	
		}
		return email;
	}
	
	
	public ReqEmail adapToCorreoSolExtValidado(SolicitudExternaDTO solicitudExt, String correo) {
		ReqEmail email =  new ReqEmail();
		if( correo != null) {
			DataEmail dataEmail  = new DataEmail();
			Map<String, Object> parametros =  new HashMap<>();
			parametros.put("NOMBRE_USUARIO", solicitudExt.getNombres() + " " + solicitudExt.getApellidoPaterno() + " " + solicitudExt.getApellidoMaterno());
			parametros.put("LINK_GDR", variablesSistema.linkSGM);
			parametros.put("CORREO_SERVIR", variablesSistema.correoGdr);
			dataEmail.setTemplateCode(Constantes.APR_SOL_ENT_EXT);
			dataEmail.setSubject(Constantes.SUBJECT_APR_SOL_ENT_EXT);
			dataEmail.setTo(correo);
			dataEmail.setBodyValues(parametros);
			email.setData(dataEmail);
			email.setIncludeAttachments(false);
		}
		return email;
	}
}
