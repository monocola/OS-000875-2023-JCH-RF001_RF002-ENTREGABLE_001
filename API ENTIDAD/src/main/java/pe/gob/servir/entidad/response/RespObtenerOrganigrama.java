package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.OrganigramaDTO;

@Getter
@Setter
public class RespObtenerOrganigrama {
	
	private List<OrganigramaDTO> listaOrganigrama;
}	
