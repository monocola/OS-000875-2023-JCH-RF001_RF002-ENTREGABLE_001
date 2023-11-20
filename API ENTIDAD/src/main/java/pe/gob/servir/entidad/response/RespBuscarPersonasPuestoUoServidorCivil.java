package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.PersonasPuestoUoServidorCivilDTO;

@Getter
@Setter
public class RespBuscarPersonasPuestoUoServidorCivil {
	
	private List<PersonasPuestoUoServidorCivilDTO> listaPersonasPuestoUoServidorCivil;
}	
