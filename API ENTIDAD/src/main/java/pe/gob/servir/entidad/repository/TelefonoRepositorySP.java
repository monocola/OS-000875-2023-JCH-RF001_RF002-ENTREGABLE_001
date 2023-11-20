package pe.gob.servir.entidad.repository;

import java.util.List;

import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.TelefonoApiDTO;

public interface TelefonoRepositorySP {
	List<TelefonoApiDTO> buscarTelefonoPersona(Long personaId, String telefono, String tipoTelefono);
	//List<TelefonoApiDTO> buscarTelefonoId(Long personaId, Long telefonoId);
	TelefonoApiDTO buscarTelefonoId(Long personaId, Long telefonoId);
	List<GenericResponseMessage> updateTelefonoSP(Long personaId, Long telefonoId, String telefono, String tipoTelefono);
	
	List<TelefonoApiDTO> buscandoTelefonoPersona(String telefono);
}
