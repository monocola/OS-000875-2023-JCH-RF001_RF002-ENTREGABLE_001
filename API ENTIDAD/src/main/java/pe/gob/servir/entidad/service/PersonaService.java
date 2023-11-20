package pe.gob.servir.entidad.service;

import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.response.RespBase;

public interface PersonaService {

	@SuppressWarnings("rawtypes")
	public RespBase<Object> obtenerInsertarPersona(int tipoPersona, Integer tipoDocumento, String nroDocumento,
			RespBase<ApiPersonaRequestDTO> request);
	
	public RespBase<Object> obtenerInsertarPersonaNuevaVersion(int tipoPersona, Integer tipoDocumento, String nroDocumento,
			RespBase<ApiPersonaRequestDTO> request);
	

	// public RespBase<RespApiPersona.Correo> insertUpdateCorreos(Long personaId,
	// 		List<ApiPersonaRequestDTO.Correo> listaCorreosNuevos, List<RespApiPersona.Correo> listaCorreos);
	
	// public RespBase<RespApiPersona.Telefono> insertUpdateTelefonos(Long personaId,
	// 		List<ApiPersonaRequestDTO.Telefono> listaTelefonosNuevos,
	// 		List<RespApiPersona.Telefono> listaTelefonosFinales);
}
