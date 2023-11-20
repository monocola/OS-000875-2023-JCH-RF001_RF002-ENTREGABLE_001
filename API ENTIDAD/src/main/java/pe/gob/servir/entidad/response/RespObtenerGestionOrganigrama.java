package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.GestionOrganigramaDTO;

@Getter
@Setter
public class RespObtenerGestionOrganigrama {
	
	private List<GestionOrganigramaDTO> listaGestionOrganigrama;
}	
