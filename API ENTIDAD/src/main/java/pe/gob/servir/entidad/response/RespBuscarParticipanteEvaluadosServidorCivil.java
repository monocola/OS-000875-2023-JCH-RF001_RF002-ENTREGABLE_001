package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ParticipanteEvaluadosServidorCivilDTO;

@Getter
@Setter
public class RespBuscarParticipanteEvaluadosServidorCivil {
	
	private List<ParticipanteEvaluadosServidorCivilDTO> listaParticipanteEvaluadosServidorCivil;
	
}	
