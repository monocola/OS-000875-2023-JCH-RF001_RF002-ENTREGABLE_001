package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PuestoActualizarDTO implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long detuoId;

	private Long uoId;
	
	private Long entidadId;
	
	private Long puestoId;
	
	private String nombrePuesto;
	
	private Integer tipoAsignacion;
	
	private Long personaIdAsignada;
	
	private String strFechaInicio;
	
	private String strFechaCese;
	
	private String usuario;
	
	@Transient
	private Date fechaInicio;

	@Transient
	private Date fechaCese;
	
}
