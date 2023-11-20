package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ServidorCivilDTO;

@Getter
@Setter
public class RespObtenerServidorCivil {
	
	private List<ServidorCivilDTO> listaServidorCivil;
}	
