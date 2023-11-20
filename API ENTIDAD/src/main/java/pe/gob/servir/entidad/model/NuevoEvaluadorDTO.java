package pe.gob.servir.entidad.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class NuevoEvaluadorDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
    
	private Long detuoId;
	private Long uoId;
	private Long personaId;
	private Long puestoId;
	private Long segmentoId;
	private Long rolId;

}
