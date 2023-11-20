package pe.gob.servir.entidad.request;

import java.util.Date;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ReqAgregarPuesto {


	@NotNull(message = Constantes.CAMPO + " uoId " + Constantes.ES_OBLIGATORIO)
	private Long uoId;

	@NotNull(message = Constantes.CAMPO + " personaId " + Constantes.ES_OBLIGATORIO)
	private Long personaId;
	
	@NotNull(message = Constantes.CAMPO + " entidadId " + Constantes.ES_OBLIGATORIO)
	private Long entidadId;
	
	@Size(max = 100, message = Constantes.CAMPO + " puesto " + Constantes.ES_INVALIDO + ", máximo 100 "
			+ Constantes.CARACTERES)
	private String puesto;
	
	@NotNull(message = Constantes.CAMPO + " puestoId " + Constantes.ES_OBLIGATORIO)
	private Long puestoId;
	
	@NotNull(message = Constantes.CAMPO + " motivoId " + Constantes.ES_OBLIGATORIO)
	private Long motivoId;
	
	@NotNull(message = Constantes.CAMPO + " fechaInicio " + Constantes.ES_OBLIGATORIO)
	private Date fechaInicio;
	
	@NotNull(message = Constantes.CAMPO + " tipoAsignacion " + Constantes.ES_OBLIGATORIO)
	private Integer tipoAsignacion;
	
	private Long personaIdAsignada;
	

}
