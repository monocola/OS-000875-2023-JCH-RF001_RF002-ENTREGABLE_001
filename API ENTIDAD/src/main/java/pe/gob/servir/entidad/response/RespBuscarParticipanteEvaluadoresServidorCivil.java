package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ParticipanteEvaluadoresServidorCivilDTO;

@Getter
@Setter
public class RespBuscarParticipanteEvaluadoresServidorCivil {
	
	private List<ParticipanteEvaluadoresServidorCivilDTO> listaParticipanteEvaluadoresServidorCivil;
}	
