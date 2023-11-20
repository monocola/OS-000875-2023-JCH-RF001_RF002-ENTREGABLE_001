package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Parametro;

@Getter
@Setter
public class RespParametro {
	private List<Parametro> listaParametros;

}
