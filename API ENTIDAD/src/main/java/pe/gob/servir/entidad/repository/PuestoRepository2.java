package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Map;

import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.model.ListaPuestoDTO;

public interface PuestoRepository2  {
	
	List<ListaPuestoDTO> listaPuesto(Map<String, Object> parametroMap) throws ValidationException;

}
