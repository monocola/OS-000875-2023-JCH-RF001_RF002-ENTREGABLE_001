package pe.gob.servir.entidad.response;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.CorreoPersonaDTO;

@Getter
@Setter
public class RespApiCorreoPersona {

	private CorreoPersonaDTO correoEvaluador;
	private CorreoPersonaDTO correoGestorORH;
	private CorreoPersonaDTO correoEvaluado;

	
}
