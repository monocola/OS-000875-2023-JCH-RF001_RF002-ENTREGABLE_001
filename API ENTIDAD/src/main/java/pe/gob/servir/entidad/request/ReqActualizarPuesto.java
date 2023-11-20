package pe.gob.servir.entidad.request;

import java.util.Date;

import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ReqActualizarPuesto {
	
	@NotNull(message = Constantes.CAMPO + " detuoId " + Constantes.ES_OBLIGATORIO)
	private Long detuoId;

	@NotNull(message = Constantes.CAMPO + " uoId " + Constantes.ES_OBLIGATORIO)
	private Long uoId;
	
	private Long entidadId;
	
	private Long puestoId;
	
	private String nombrePuesto;
	
	@NotNull(message = Constantes.CAMPO + " tipoAsignacion " + Constantes.ES_OBLIGATORIO)
	private Integer tipoAsignacion;
	
	private Long motivoId;
	
	private Long personaIdAsignada;

	@NotNull(message = Constantes.CAMPO + " fechaInicio " + Constantes.ES_OBLIGATORIO)
	private String strFechaInicio;
		
	private Date fechaCese;
	
	@Transient
	private Date fechaInicio;

}
