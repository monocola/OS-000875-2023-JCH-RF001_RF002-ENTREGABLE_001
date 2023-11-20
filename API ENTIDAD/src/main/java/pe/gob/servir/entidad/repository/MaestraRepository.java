package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Map;

import pe.gob.servir.entidad.model.ListaMaestrasDTO;

public interface MaestraRepository {
	
	List<ListaMaestrasDTO> obtenerParametro(Map<String, Object> parametroMap);

	
}
