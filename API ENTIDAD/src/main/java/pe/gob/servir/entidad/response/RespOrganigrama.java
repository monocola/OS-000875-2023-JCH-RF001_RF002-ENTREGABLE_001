package pe.gob.servir.entidad.response;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Organigrama;

@Getter
@Setter
public class RespOrganigrama {

	private Organigrama organigrama;
	
	private RespApiPersona persona;
	
}
