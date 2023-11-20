package pe.gob.servir.entidad.model;

import java.io.Serializable;
import java.time.LocalDate;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DetalleUoDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private Long detalleUoId;
	private Long organigramaId;
	private Long entidadId;
	private Long personaId;
	private Long puestoId;
	private String responsable;
	private String excluye;
	private String estadoRegistro;
	private String usuarioCreacion;
	private LocalDate fechaCreacion;
	private String usuarioModificacion;
	private LocalDate fechaModificacion;
	private LocalDate puestoFechaInicio;
	private LocalDate fechaCese;
	private Integer tipoAsignacion;

}
