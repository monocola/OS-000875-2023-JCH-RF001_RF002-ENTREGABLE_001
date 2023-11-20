package pe.gob.servir.entidad.request.dto;

import javax.validation.constraints.Size;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class DireccionDTO {

	private Long direccionId;
	private Integer ubigeoId;
	@Size(max = 200, message = "Campo referenciaDireccion es inválido, máximo 200 caracteres")
	private String referenciaDireccion;
	private Integer zonaGeografica;
	
}
