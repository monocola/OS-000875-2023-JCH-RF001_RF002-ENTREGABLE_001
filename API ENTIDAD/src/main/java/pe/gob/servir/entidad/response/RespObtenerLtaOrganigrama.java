package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Organigrama;

@Getter
@Setter
public class RespObtenerLtaOrganigrama {
	
	private List<Organigrama> listaOrganigrama;
}
