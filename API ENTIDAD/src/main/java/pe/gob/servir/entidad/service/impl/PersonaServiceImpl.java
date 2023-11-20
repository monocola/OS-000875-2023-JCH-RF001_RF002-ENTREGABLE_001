package pe.gob.servir.entidad.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.model.CorreoApiDTO;
import pe.gob.servir.entidad.repository.CorreoRepositorySP;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class PersonaServiceImpl implements PersonaService {

	@Autowired
	PersonaApiClient personaApiClient;
	
	@Autowired
	CorreoRepositorySP correoRepositorySP;
	
	@Autowired
	VariablesSistema variablesSistema;
	

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public RespBase<Object> obtenerInsertarPersona(int tipoPersona, Integer tipoDocumento, String nroDocumento,
			RespBase<ApiPersonaRequestDTO> request) {
		RespBase<Object> responseResultado = new RespBase<Object>();
		try {
			RespBase<RespApiPersona> response = personaApiClient.obtienePersonaPorDocumento(tipoDocumento,
					nroDocumento);
			
			if (response.getStatus().getSuccess()) {
				if (response.getPayload() == null || response.getPayload().getPersona() == null) {
					if (tipoPersona == variablesSistema.tipoPersonaJuridico) {
						
						response = personaApiClient.registrarPersonaJuridica(request);
					} else {
						response = personaApiClient.registrarPersonaNatural(request);
					}
					if (response.getStatus().getSuccess()) {
						response = personaApiClient.obtienePersonaPorDocumento(tipoDocumento, nroDocumento);
					}
				} else {
					RespApiPersona personaResponse = response.getPayload();
					List<RespApiPersona.Telefono> listaTelefonos = new ArrayList<RespApiPersona.Telefono>();
					List<RespApiPersona.Correo> listaCorreos = new ArrayList<RespApiPersona.Correo>();
					ApiPersonaRequestDTO personaRequest = request.getPayload();
					List<ApiPersonaRequestDTO.Correo> listaCorreosNuevos = personaRequest.getCorreos();
					List<ApiPersonaRequestDTO.Telefono> listaTelfonosNuevos = personaRequest.getTelefonos();
					setIdTelefonosExistentes(personaResponse.getTelefonos(), listaTelfonosNuevos);
					setIdCorreosExistentes(personaResponse.getCorreos(), listaCorreosNuevos);
					RespBase<RespApiPersona.Telefono> responseTelefono = insertUpdateTelefonos(
							personaResponse.getPersona().getPersonaId(), listaTelfonosNuevos, listaTelefonos);
					if (responseTelefono.getStatus().getSuccess()) {
						RespBase<RespApiPersona.Correo> responseCorreo = insertUpdateCorreos(
								personaResponse.getPersona().getPersonaId(), listaCorreosNuevos, listaCorreos);
						if (responseCorreo.getStatus().getSuccess()) {
							personaResponse.setTelefonos(listaTelefonos);
							personaResponse.setCorreos(listaCorreos); 
						} else
							response.setStatus(responseCorreo.getStatus());
					} else {
						response.setStatus(responseTelefono.getStatus());
					}
				}
			}
			responseResultado.setPayload(response.getPayload());
			responseResultado.setStatus(response.getStatus());
			responseResultado.setTrace(response.getTrace());
		} catch (Exception e) {
			responseResultado.getStatus().setSuccess(Boolean.FALSE);
			responseResultado.getStatus().getError().getMessages().add(ParametrosUtil.obtenerMensajeError(e.getMessage()));
		}
		return responseResultado;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public RespBase<Object> obtenerInsertarPersonaNuevaVersion(int tipoPersona, Integer tipoDocumento, String nroDocumento,
			RespBase<ApiPersonaRequestDTO> request) {
		RespBase<Object> responseResultado = new RespBase<Object>();
		try {
			List<ApiPersonaRequestDTO.Telefono> listaTelfonosNuevos = new ArrayList<>(request.getPayload().getTelefonos());
			request.getPayload().getTelefonos().clear();
			List<ApiPersonaRequestDTO.Correo> listaCorreosNuevos = new ArrayList<>(request.getPayload().getCorreos());
			request.getPayload().getCorreos().clear();
			RespBase<RespApiPersona> response = personaApiClient.obtienePersonaPorDocumento(tipoDocumento,
					nroDocumento);
			if (response.getStatus().getSuccess()) {
				if (response.getPayload() == null || response.getPayload().getPersona() == null) {
					if (tipoPersona == variablesSistema.tipoPersonaJuridico) {
						response = personaApiClient.registrarPersonaJuridica(request);
					} else {
						response = personaApiClient.registrarPersonaNatural(request);
					}
					if (response.getStatus().getSuccess()) {
						response = personaApiClient.obtienePersonaPorDocumento(tipoDocumento, nroDocumento);
					}
				}
				if (response.getStatus().getSuccess()) {
					RespApiPersona personaResponse = response.getPayload();
					if (listaTelfonosNuevos != null && listaTelfonosNuevos.size() > 0) {
						List<RespApiPersona.Telefono> listaTelefonosFinales = new ArrayList<RespApiPersona.Telefono>();
						setIdTelefonosExistentes(personaResponse.getTelefonos(), listaTelfonosNuevos);
						RespBase<RespApiPersona.Telefono> responseTelefono = insertUpdateTelefonos(
								personaResponse.getPersona().getPersonaId(), listaTelfonosNuevos, listaTelefonosFinales);
						if (responseTelefono.getStatus().getSuccess()) {
							personaResponse.setTelefonos(listaTelefonosFinales);
						}else {
							response.setStatus(responseTelefono.getStatus());
						}
					}
					if (response.getStatus().getSuccess()) {
						if (listaCorreosNuevos != null && listaCorreosNuevos.size() > 0) {
							List<RespApiPersona.Correo> listaCorreosFinales = new ArrayList<RespApiPersona.Correo>();
							setIdCorreosExistentes(personaResponse.getCorreos(), listaCorreosNuevos);
							RespBase<RespApiPersona.Correo> responseCorreo = insertUpdateCorreos(
									personaResponse.getPersona().getPersonaId(), listaCorreosNuevos, listaCorreosFinales);
							if (responseCorreo.getStatus().getSuccess()) {
								personaResponse.setCorreos(listaCorreosFinales);
							}else {
								response.setStatus(responseCorreo.getStatus());
							}
						}
					}
					if (response.getStatus().getSuccess()) {
						response = personaApiClient.obtienePersonaPorDocumento(tipoDocumento, nroDocumento);
					}
				}
			}
			responseResultado.setPayload(response.getPayload());
			responseResultado.setStatus(response.getStatus());
			responseResultado.setTrace(response.getTrace());
		} catch (Exception e) {
			responseResultado.getStatus().setSuccess(Boolean.FALSE);
			responseResultado.getStatus().getError().getMessages().add(e.getMessage());
		}
		return responseResultado;
	}

	public static void setIdTelefonosExistentes(List<RespApiPersona.Telefono> listaTelefonosActuales,
			List<ApiPersonaRequestDTO.Telefono> listaTelefonosNuevos) {
		if (listaTelefonosNuevos != null) {
			for (ApiPersonaRequestDTO.Telefono telefonoNuevo : listaTelefonosNuevos) {
				if (listaTelefonosActuales != null) {
					for (RespApiPersona.Telefono telefonoActual : listaTelefonosActuales) {
						if (telefonoNuevo.getNumeroTelefono().equals(telefonoActual.getNumeroTelefono())) {
							telefonoNuevo.setTelefonoId(telefonoActual.getTelefonoId());
						}
					}
				}
			}
		}
	}

	public static void setIdCorreosExistentes(List<RespApiPersona.Correo> listaCorreos,
			List<ApiPersonaRequestDTO.Correo> listaCorreosNuevos) {
		if (listaCorreosNuevos != null) {
			for (ApiPersonaRequestDTO.Correo correoNuevo : listaCorreosNuevos) {
				if (listaCorreos != null) {
					for (RespApiPersona.Correo correo : listaCorreos) {
						if (correoNuevo.getCorreo().equals(correo.getCorreo())) {
							correoNuevo.setCorreoId(correo.getCorreoId());
						}
					}
				}
			}
		}
	}

	public RespBase<RespApiPersona.Telefono> insertUpdateTelefonos(Long personaId,
			List<ApiPersonaRequestDTO.Telefono> listaTelefonosNuevos,
			List<RespApiPersona.Telefono> listaTelefonosFinales) {
		RespBase<RespApiPersona.Telefono> response = new RespBase<RespApiPersona.Telefono>();
		response.getStatus().setSuccess(Boolean.TRUE);
		for (ApiPersonaRequestDTO.Telefono telefono : listaTelefonosNuevos) {
			RespBase<ApiPersonaRequestDTO.Telefono> apiTelefono = new RespBase<ApiPersonaRequestDTO.Telefono>();
			apiTelefono.setPayload(telefono);
			if (telefono.getTelefonoId() == null) {
				response = personaApiClient.crearTelefono(personaId.intValue(), apiTelefono);
			} else {
				response = personaApiClient.actualizaTelefono(telefono.getTelefonoId().toString(), apiTelefono);
			}
			if (response.getStatus().getSuccess()) {
				RespApiPersona.Telefono telefonoResponse = response.getPayload();
				telefono.setTelefonoId(telefonoResponse.getTelefonoId());
				listaTelefonosFinales.add(telefonoResponse);
			} else {
				return response;
			}
		}
		return response;
	}

	public RespBase<RespApiPersona.Correo> insertUpdateCorreos(Long personaId,
			List<ApiPersonaRequestDTO.Correo> listaCorreosNuevos, List<RespApiPersona.Correo> listaCorreos) {
		RespBase<RespApiPersona.Correo> response = new RespBase<RespApiPersona.Correo>();
		response.getStatus().setSuccess(Boolean.TRUE);
		for (ApiPersonaRequestDTO.Correo correo : listaCorreosNuevos) {
			RespBase<ApiPersonaRequestDTO.Correo> apiCorreo = new RespBase<ApiPersonaRequestDTO.Correo>();
			apiCorreo.setPayload(correo);
			List<CorreoApiDTO> correoRpta = correoRepositorySP.buscarEmailPersona(correo.getCorreo());
			if(correoRpta == null || correoRpta.size() == 0){
				if (correo.getCorreoId() == null) {
					response = personaApiClient.crearCorreo(personaId.intValue(), apiCorreo);
				} else {
					response = personaApiClient.actualizaCorreo(correo.getCorreoId().toString(), apiCorreo);
				}
			}else {
				RespApiPersona.Correo correoExistente = new RespApiPersona.Correo();
				correoExistente.setCorreo(correoRpta.get(0).getCorreo());
				correoExistente.setCorreoId(correoRpta.get(0).getCorreoId());
				correoExistente.setPersonaId(personaId);
				correoExistente.setTipoCorreo(correoRpta.get(0).getTipoCorreo());
				response.setPayload(correoExistente);
				response.getStatus().setSuccess(Boolean.TRUE);
			}
			if (response.getStatus().getSuccess()) {
				RespApiPersona.Correo correoResponse = response.getPayload();
				correo.setCorreoId(correoResponse.getCorreoId());
				listaCorreos.add(correoResponse);
			} else {
				return response;
			}
		}
		return response;
	}
}
