package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ComboPuesto;

@Getter
@Setter
public class RespComboPuesto {
	
	private List<ComboPuesto> listaComboPuesto;
}
