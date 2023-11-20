package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ListaSedeDTO;

@Getter
@Setter
public class RespObtenerSede {
	List<ListaSedeDTO> listarSede;

}
