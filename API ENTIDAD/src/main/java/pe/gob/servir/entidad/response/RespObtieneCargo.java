package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Cargo;

@Getter
@Setter
public class RespObtieneCargo {
	
	private List<Cargo> listaCargo;
}
