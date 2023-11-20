package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.PuestoUOxEvaluador;


@Getter
@Setter
public class RespListarDetalleUO {

	private List<PuestoUOxEvaluador> listaPuesto;
}	
