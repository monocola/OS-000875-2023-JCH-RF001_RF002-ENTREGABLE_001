package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Map;

import pe.gob.servir.entidad.model.EstadoSolicitud;
import pe.gob.servir.entidad.model.ObtenerSolicitud;

public interface SolicitudRepositorySP {
	
	List<ObtenerSolicitud> buscarSolicitudFilter(Map<String, Object> parametroMap);
	List<EstadoSolicitud> buscarSolicitudByNro(String nro);
	
}
