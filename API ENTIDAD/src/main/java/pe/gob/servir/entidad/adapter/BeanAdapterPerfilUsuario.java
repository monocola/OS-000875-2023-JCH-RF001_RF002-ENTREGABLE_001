package pe.gob.servir.entidad.adapter;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.PerfilUsuarioDTO;
import pe.gob.servir.entidad.response.RespObtienePerfilUsuario;
import pe.gob.servir.entidad.util.FilesUtil;

@Component
public class BeanAdapterPerfilUsuario {

	public void adapToBeanResponsePerfilUsuario(RespObtienePerfilUsuario out, PerfilUsuarioDTO inServidor) {

		out.setPersonaId(inServidor.getPersonaId());
		out.setEntidadId(inServidor.getEntidadId());
		out.setApellidoPaterno(StringUtils.trimToEmpty(inServidor.getApellidoPaterno()));
		out.setApellidoMaterno(StringUtils.trimToEmpty(inServidor.getApellidoMaterno()));
		out.setNombres(StringUtils.trimToEmpty(inServidor.getNombres()));
		out.setTipoDocumento(StringUtils.trimToEmpty(inServidor.getTipoDocumento()));
		out.setNumeroDocumento(inServidor.getNumeroDocumento());
		out.setTelefono(StringUtils.trimToEmpty(inServidor.getTelefono()));
		out.setGenero(StringUtils.trimToEmpty(inServidor.getGenero()));
		out.setFechaNacimiento(inServidor.getFechaNacimiento() == null ? null
				: FilesUtil.formatDateToString(inServidor.getFechaNacimiento()));
		out.setCorreoInstitucional(StringUtils.trimToEmpty(inServidor.getCorreoInstitucional()));
		out.setCorreoAlternativo(StringUtils.trimToEmpty(inServidor.getCorreoAlternativo()));
		out.setRegimenLaboral(StringUtils.trimToEmpty(inServidor.getRegimenLaboral()));
		out.setSindicato(StringUtils.trimToEmpty(inServidor.getSindicato()));
		out.setRegimenLaboralId(inServidor.getRegimenId());

		String ruta = inServidor.getUrlFoto();
		if (Constantes.VACIO.equals(StringUtils.trimToEmpty(ruta))) {
			out.setUrlFoto(Constantes.VACIO);
		} else {
			out.setUrlFoto(inServidor.getUrlFoto());
		}
	}
}