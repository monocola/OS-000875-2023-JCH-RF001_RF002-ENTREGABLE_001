package pe.gob.servir.entidad.request.dto;

import javax.validation.constraints.Size;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class MedioContactoDTO {

	private Long medioContactoId;
	private Long entidadId;
	private String tipoMedioContacto;
	private String flagPrincipal;
	@Size(max = 100, message = "Campo nombreRealArchivo es inválido, máximo 100 caracter")
	private String valorMedioContacto;
	@Size(max = 5, message = "Campo nombreRealArchivo es inválido, máximo 5 caracter")
	private String anexoMedioContacto;
}
