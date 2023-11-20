package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PuestoAgregarDTO implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private Long detUoId;
	
	private Long uoId;
	
	private Long entidadId;
	
	private Long personaId;
	
	private Long puestoId;
	
	private String puesto;
	
	private Date fechaInicio;
	
	private Integer tipoAsignacion;
	
	private Long idDetUOPersonaAsingada;
	
	private Long personaIdAsignada;
	
	private Long motivoId;
	
}
