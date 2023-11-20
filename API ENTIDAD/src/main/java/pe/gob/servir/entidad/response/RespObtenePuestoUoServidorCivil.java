package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.PuestoUoServidorCivilDTO;

@Getter
@Setter
public class RespObtenePuestoUoServidorCivil {
	
	private List<PuestoUoServidorCivilDTO> listaPuestoUoServidorCivil;
}	
