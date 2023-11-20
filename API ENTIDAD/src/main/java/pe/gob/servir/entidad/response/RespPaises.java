package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.PaisesDTO;

@Getter
@Setter
public class RespPaises {
	private List<PaisesDTO> listaPaises;

}
