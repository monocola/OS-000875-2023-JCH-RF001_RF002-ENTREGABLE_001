package pe.gob.servir.entidad.request.dto;
import javax.validation.constraints.Size;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class SolicitudDTO {
	
	private Long solicitudEntidadId;
	private Long sector;
	private Long nivelGobierno;
	private Integer tipo;
	private Integer tipoEntidad;
	@Size(max = 1, message = "Campo aceptoTerminosCondiciones es inválido, máximo 1 caracter")
	private String aceptoTerminosCondiciones;
	private Integer numeroIntentos;
	private Integer aplicacionId;
	private Integer estadoSolicitud;

}
