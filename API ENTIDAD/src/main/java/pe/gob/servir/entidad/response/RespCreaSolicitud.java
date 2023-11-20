package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Solicitud;
import pe.gob.servir.entidad.model.SolicitudArchivo;
import pe.gob.servir.entidad.model.SolicitudPersona;

@Getter
@Setter
public class RespCreaSolicitud {
	
	private Solicitud solicitudEntidad;
		
	private List<SolicitudArchivo> listaSolicitudArchivo;
	
	private List<SolicitudPersona>  listaSolicitudPersona;
	
	private Boolean solicitudExistente;
}
