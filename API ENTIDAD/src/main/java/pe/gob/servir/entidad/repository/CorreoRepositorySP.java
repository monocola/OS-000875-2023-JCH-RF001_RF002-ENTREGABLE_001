package pe.gob.servir.entidad.repository;

import java.util.List;

import pe.gob.servir.entidad.model.CorreoApiDTO;

public interface CorreoRepositorySP {
	List<CorreoApiDTO> buscarEmailPersona(String correo);
}
