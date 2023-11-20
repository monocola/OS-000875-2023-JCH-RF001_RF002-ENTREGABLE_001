package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.NuevoEvaluadorDTO;

@Getter
@Setter
public class ReqActualizaNuevoEvaluadorServidorCivil {

	private Long entidadId;
	private Long uoId;
	private Long evaluadorMMdetUoId;
	
	@Valid
	private List<NuevoEvaluadorDTO> evaluados;
	
}
