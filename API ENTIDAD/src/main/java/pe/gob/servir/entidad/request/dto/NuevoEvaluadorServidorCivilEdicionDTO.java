package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.NuevoEvaluadorDTO;

@Getter
@Setter
public class NuevoEvaluadorServidorCivilEdicionDTO implements Serializable {

	private static final long serialVersionUID = 1L;

	
	private Long entidadId;
	private Long uoId;
	private Long evaluadorMMdetUoId;
	
	private Long evaluadoDetUoId;
	
	private String usuario;
	private List<NuevoEvaluadorDTO> evaluados;
	
	
	
}
