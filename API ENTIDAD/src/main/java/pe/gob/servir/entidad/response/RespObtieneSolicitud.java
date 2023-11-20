package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ObtenerSolicitud;

@Getter
@Setter
public class RespObtieneSolicitud {
	
	private List<ObtenerSolicitud> listaObtenerSolicitud;
	
}
