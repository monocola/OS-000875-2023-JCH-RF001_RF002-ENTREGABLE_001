package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PuestoDTO implements Serializable {

	private static final long serialVersionUID = 1L;
	private Long entidadId;
	private String descripcion;
	private String estadoRegistro;
	private Long organigramaId;
	private String esJefe;
	
	
}
