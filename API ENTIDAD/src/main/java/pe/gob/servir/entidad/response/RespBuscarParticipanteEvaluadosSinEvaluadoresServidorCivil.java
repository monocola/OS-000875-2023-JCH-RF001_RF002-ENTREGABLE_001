package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ParticipanteEvaluadosServidorCivilDTO;

@Getter
@Setter
public class RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil {
	
	private Integer cantidadSinEvaluar;
	
	private List<ParticipanteEvaluadosServidorCivilDTO> listaParticipanteEvaluadosSinEvaluador;
}	
