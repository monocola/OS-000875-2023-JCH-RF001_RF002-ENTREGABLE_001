package pe.gob.servir.entidad.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PuestoUOxEvaluador implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String descripcion;
	private String sigla;
	private Long detalleUoId;
	private Long organigramaId;
	private Long entidadId;
	private Long personaId;
	private Long puestoId;
	private String responsable;
	private Long rolId;
	private Long evaluadoDetalleUoId;

}
