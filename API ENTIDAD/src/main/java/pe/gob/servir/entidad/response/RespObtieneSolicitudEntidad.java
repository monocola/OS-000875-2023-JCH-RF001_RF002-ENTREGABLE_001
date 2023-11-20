package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Solicitud;
@Getter
@Setter
public class RespObtieneSolicitudEntidad {

	private List<Solicitud> listaSolicitud;
	
}
