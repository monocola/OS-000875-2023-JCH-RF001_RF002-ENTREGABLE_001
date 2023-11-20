package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ParticipanteServidorCivilDTO;

@Getter
@Setter
public class RespBuscarParticipanteServidorCivil {
	
	private List<ParticipanteServidorCivilDTO> listaParticipanteServidorCivil;
}	
