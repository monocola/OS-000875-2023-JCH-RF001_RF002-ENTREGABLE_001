package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ListaPuestoDTO;

@Getter
@Setter
public class RespListarPuesto {

	private List<ListaPuestoDTO> listaPuesto;
}	
