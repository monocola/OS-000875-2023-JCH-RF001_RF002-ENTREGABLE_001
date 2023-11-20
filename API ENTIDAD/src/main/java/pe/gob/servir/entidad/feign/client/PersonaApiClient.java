package pe.gob.servir.entidad.feign.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import pe.gob.servir.entidad.api.dto.ApiActualizarPersonaJuridica;
import pe.gob.servir.entidad.api.dto.ApiActualizarPersonaNatural;
import pe.gob.servir.entidad.api.dto.ApiBuscarCorreo;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespApiPersonaCorreo;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerCorreo;
import pe.gob.servir.entidad.response.RespObtenerTelefono;

@FeignClient(name = "personaApi", url = "${jboss.private.base.url.persona}")
public interface PersonaApiClient {

	// @formatter:off
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_PERSONA_BY_DOCUMENTO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> obtienePersonaPorDocumento(
			@RequestParam(value = "tipoDocumento") Integer tipoDocumento,
			@RequestParam(value = "numeroDocumento") String numeroDocumento);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_PERSONA_BY_DOCUMENTO_V3, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> obtienePersonaPorDocumentoReniec(
			@RequestParam(value = "tipoDocumento") Integer tipoDocumento,
			@RequestParam(value = "numeroDocumento") String numeroDocumento);
	
	@SuppressWarnings("rawtypes")
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_INS_PERSONA_JURIDICA, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> registrarPersonaJuridica(RespBase<ApiPersonaRequestDTO> request);
	
	
	@SuppressWarnings("rawtypes")
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_INS_PERSONA_NATURAL, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> registrarPersonaNatural(RespBase<ApiPersonaRequestDTO> request);
	
	@SuppressWarnings("rawtypes")
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_VAL_PERSONA_NATURAL, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<String> validarPersonaNatural(RespBase<ApiPersonaRequestDTO> request);
	
	@SuppressWarnings("rawtypes")
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_BUSCAR_PERSONA_CORREO, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersonaCorreo> buscarCorreo(RespBase<ApiBuscarCorreo> request);
	
	
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_CREAR_TELEFONO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona.Telefono> crearTelefono(
			@PathVariable("personaId") Integer personaId,
			RespBase<ApiPersonaRequestDTO.Telefono> request);
	
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_CREAR_CORREO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona.Correo> crearCorreo(
			@PathVariable("personaId") Integer personaId,
			RespBase<ApiPersonaRequestDTO.Correo> request);
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_TELEFONO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona.Telefono> actualizaTelefono(
			@PathVariable("telefonoId") String telefonoId,
			RespBase<ApiPersonaRequestDTO.Telefono> request);	
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_CORREO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona.Correo> actualizaCorreo(
			@PathVariable("correoId") String correoId,
			RespBase<ApiPersonaRequestDTO.Correo> request);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_TELEFONO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona.Telefono> obtenerTelefono(
			@RequestParam(value = "telefonoId") Integer telefonoId);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_CORREO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona.Correo> obtenerCorreo(
			@RequestParam(value = "correoId") Integer correoId);
	// @formatter:on
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_PERSONA_BY_ID, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> obtenerPersonaById(
			@RequestParam(value = "personaId") Integer personaId); 
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_TELEFONO_BY_PERSONAID, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespObtenerTelefono> obtenerTelefonoBypersonaId(
			@RequestParam(value = "personaId") Integer personaId); 
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_CORREO_BY_PERSONAID, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespObtenerCorreo> obtenerCorreoBypersonaId(
			@RequestParam(value = "personaId") Integer personaId); 

	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_UPDATE_FECHANAC, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> actualizaPersonaNatural(
			@PathVariable("personaId") Long personaId,
			RespBase<ApiActualizarPersonaNatural> request);
	
	//	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_UPDATE_PER_JURIDICA, 
	//			produces = {MediaType.APPLICATION_JSON_VALUE },
	//			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	//	RespBase<RespApiPersona> actualizaPersonaJuridica(
	//			@PathVariable("personaId") Long personaId,
	//			RespBase<ApiActualizarPersonaJuridica> request);

	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_UPDATE_PERSONAS_JURIDICA, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiPersona> actualizaPersonaJuridica(
			@PathVariable("personaId") Long personaId,
			RespBase<ApiActualizarPersonaJuridica> request);
}
