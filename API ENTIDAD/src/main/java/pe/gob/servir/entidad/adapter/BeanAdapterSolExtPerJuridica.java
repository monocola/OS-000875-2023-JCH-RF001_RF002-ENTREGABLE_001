package pe.gob.servir.entidad.adapter;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;

@Component
public class BeanAdapterSolExtPerJuridica {

	public ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaJuridica> adapSolicitudExtToJuridico (RespBase<RespApiPersona> apiPersona, SolicitudExternaDTO solicitudExt, int TIPO_DOCUMENTO_RUC) {
		ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaJuridica> apiPersonaResp = new ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaJuridica>();
		ApiPersonaRequestDTO.PersonaJuridica persona = new ApiPersonaRequestDTO.PersonaJuridica();
		
		try {
			
			if(solicitudExt.getNombreEntidad() != "" && solicitudExt.getNombreEntidad() != null ) {
				persona.setNombreComercial(solicitudExt.getNombreEntidad().trim());
			}
			
			if(solicitudExt.getRazonSocial() != "" && solicitudExt.getRazonSocial() != null ) {
				persona.setRazonSocial(solicitudExt.getRazonSocial().trim());
			}
			
			apiPersonaResp.setPersona(persona);
			if (apiPersona.getPayload().getDirecciones().get(0).getUbigeoId() != null) {
				Long ubigeoId = apiPersona.getPayload().getDirecciones().get(0).getUbigeoId();
				String direccionCompleta = apiPersona.getPayload().getDirecciones().get(0).getDireccionCompleta();
				String referencia = apiPersona.getPayload().getDirecciones().get(0).getReferencia();
				apiPersonaResp.getDirecciones().add( 
						new ApiPersonaRequestDTO.Direccion(ubigeoId, null,
						(!StringUtils.isEmpty(direccionCompleta) ? direccionCompleta : "-"),
						referencia) );
			}
		
			if (!StringUtils.isEmpty(solicitudExt.getNumeroDocumento())) {
				apiPersonaResp.getDocumentos().add(new ApiPersonaRequestDTO.Documentos(TIPO_DOCUMENTO_RUC,
						solicitudExt.getRucEntidad()));
			}
			
			if (!StringUtils.isEmpty(solicitudExt.getTelefonoFijo())) {	
				apiPersonaResp.getTelefonos().add(new ApiPersonaRequestDTO.Telefono(Constantes.TIPO_TELEFONO_CASA, null,
						solicitudExt.getTelefonoFijo(), null));
			}
			
			if (!StringUtils.isEmpty(solicitudExt.getCelular())) {
				apiPersonaResp.getTelefonos().add(new ApiPersonaRequestDTO.Telefono(Constantes.TIPO_TELEFONO_CELULAR, null,
						solicitudExt.getCelular(), null));
			}	
			// if (!StringUtils.isEmpty(solicitudExt.getRutaPaginaWeb())) {
			//	apiPersona.getWebs().add(new ApiPersonaRequestDTO.Web(solicitudPersona.getRutaPaginaWeb()));
			// }
			
			// if (!StringUtils.isEmpty(solicitudExt.getCorreoPrincipal())) {
			//	apiPersonaResp.getCorreos().add(new ApiPersonaRequestDTO.Correo(null, Constantes.TIPO_CORREO_PRINCIPAL,	solicitudExt.getCorreoPrincipal()));
			// }
		} catch (Exception e) {
			return null;
			// TODO: handle exception
		}
		
	
		
		return apiPersonaResp;
	}
	
}
