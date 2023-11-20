package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ListaMaestrasDTO;

@Getter
@Setter
public class RespObtenerParametro {
	
	private List<ListaMaestrasDTO> listaMaestra;
}	
