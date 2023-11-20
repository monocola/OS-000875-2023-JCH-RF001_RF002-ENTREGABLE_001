package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.model.ListaEntidadDTO;
import pe.gob.servir.entidad.model.ResumenServidoresCivilesDTO;

@Getter
@Setter
@ToString
public class RespListaServidoresCiviles {
	
	private List<ResumenServidoresCivilesDTO>  servidoresCiviles;
	
	

}
