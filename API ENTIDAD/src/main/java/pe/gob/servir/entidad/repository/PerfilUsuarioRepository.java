package pe.gob.servir.entidad.repository;


import java.util.List;
import java.util.Map;

import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.model.PerfilUsuarioDTO;

public interface PerfilUsuarioRepository {

	List<PerfilUsuarioDTO> obtenerPerfilUsuario(Map<String, Object> parametroMap) throws ValidationException;

}