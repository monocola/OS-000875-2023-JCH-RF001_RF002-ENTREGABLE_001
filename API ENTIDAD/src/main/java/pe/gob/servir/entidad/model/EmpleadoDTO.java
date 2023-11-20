package pe.gob.servir.entidad.model;

import java.io.Serializable;
import java.time.Instant;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class EmpleadoDTO implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Long entidadId;
	private Long personaId;
	private Long regimenLaboral;
	private Long puestoId;
	private String sindicatoFlag;
	private String estadoRegistro;
	private String usuarioCreacion;
	private Instant fechaCreacion;
}
