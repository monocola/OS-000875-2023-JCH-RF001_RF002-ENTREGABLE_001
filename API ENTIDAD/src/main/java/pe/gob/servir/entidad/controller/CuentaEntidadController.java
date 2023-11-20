package pe.gob.servir.entidad.controller;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.request.ReqActualizaCuentaEntidad;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqCreaCuentaEntidad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespComboUsuarioPorEntidadRol;
import pe.gob.servir.entidad.response.RespInactivarCuenta;
import pe.gob.servir.entidad.response.RespListaEntidades;
import pe.gob.servir.entidad.response.RespValidaTarea;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.CuentaEntidadService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@RestController
@Tag(name = "CuentaEntidad", description = "")
public class CuentaEntidadController {
	
	@Autowired
	private HttpServletRequest httpServletRequest;
	
	@Autowired
	private VariablesSistema variablesSistema;
	
	@Autowired
	private CuentaEntidadService cuentaEntidadService;
	
	@Operation(summary = Constantes.SUM_OBT_LIST+"cuenta entidades", description = Constantes.SUM_OBT_LIST+"cuenta entidades", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/cuentaentidad/filter"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaEntidades>> listarEntidadesByRol(
			@PathVariable String access,
			@RequestParam(value = "idEntidad", required = false) Integer idEntidad,@RequestParam(value = "texto", required = false) String texto
			) {
		Map<String, Object> parametroMap = new HashMap<>();	
		parametroMap.put("cuentaId", idEntidad);
		parametroMap.put("aplicacionId", variablesSistema.aplicacionTalentoId.intValue());
		parametroMap.put("texto", texto);			
		
		RespBase<RespListaEntidades> response = cuentaEntidadService.buscarEntidadesAsociadas(parametroMap);
		
		return ResponseEntity.ok(response);		
	}
	
		@Operation(summary = "Crea una Cuenta Entidad", description = "Crea una Cuenta Entidad", tags = { "" },
				security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT+"/cuentaentidad" }, 
				 consumes = {MediaType.APPLICATION_JSON_VALUE },
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
		public ResponseEntity<RespBase<Object>> creaCuentaEntidad(
			@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqCreaCuentaEntidad> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<Object> response = cuentaEntidadService.registrarCuentEnti(request, jwt);
		return ResponseEntity.ok(response);	
	}
		
	@Operation(summary = "Actualizar una Cuenta Entidad", description = "Actualizar una Cuenta Entidad", tags = { "" },
				security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/cuentaentidad/{idCuentaEntidad}" }, 
				 consumes = {MediaType.APPLICATION_JSON_VALUE },
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
		public ResponseEntity<RespBase<Object>> actualizarCuentaEntidad(
			@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqActualizaCuentaEntidad> request,
			@PathVariable Long idCuentaEntidad) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<Object> response = cuentaEntidadService.actualizarCuentaEntidad(request, jwt, idCuentaEntidad);
		return ResponseEntity.ok(response);	
	}
	
	@Operation(summary = "Inactivar una Cuenta Entidad", description = "Inactivar una Cuenta Entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
@ApiResponses(value = { 
		@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
		@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
				@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
		@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
				@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
@DeleteMapping(path = { Constantes.BASE_ENDPOINT+"/cuentaentidad/{idCuentaEntidad}" }, 			
			 produces = { MediaType.APPLICATION_JSON_VALUE })	
	public ResponseEntity<RespBase<RespInactivarCuenta>> inactivarCuentaEntidad(
		@PathVariable String access,
		@PathVariable Long idCuentaEntidad,
		@RequestParam(value = "estado", required = false) String estado) {
		
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<RespInactivarCuenta> responseCuenta = cuentaEntidadService.inactivarCuentaEntidad(idCuentaEntidad,estado,jwt);
		if(!responseCuenta.getStatus().getSuccess()) {
			responseCuenta = ParametrosUtil.setearResponse(responseCuenta,Boolean.FALSE,Constantes.RESPONSE_MESSAGE);
		}
		return ResponseEntity.ok(responseCuenta);	
	}
	
	@Operation(summary = "Validar una Cuenta Entidad", description = "Validar una Cuenta Entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
@ApiResponses(value = { 
		@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
		@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
				@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
		@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
				@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
@GetMapping(path = { Constantes.BASE_ENDPOINT+"/cuentaentidad/{idCuentaEntidad}" }, 
			 produces = { MediaType.APPLICATION_JSON_VALUE })	
	public ResponseEntity<RespBase<RespValidaTarea>> validarCuentaEntidad(
		@PathVariable String access,
		@PathVariable Long idCuentaEntidad) {

	RespBase<RespValidaTarea> response = cuentaEntidadService.validaTareaCuentaEntidad(idCuentaEntidad);
	return ResponseEntity.ok(response);	
}
		
	@Operation(summary = "Consultar las Cuentas asociadas a una Entidad y por un Rol", description = "Consultar las Cuentas asociadas a una Entidad y por un Rol", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/cuentaentidad/persona" }, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespComboUsuarioPorEntidadRol>> listarCuentasPorEntidadPorRol(
		@PathVariable String access,
		@RequestParam(value = "idEntidad", required = false) Integer idEntidad,@RequestParam(value = "idRol", required = false) Integer idRol) {

		Map<String, Object> parametroMap = new HashMap<>();	
		parametroMap.put("entidadId", idEntidad);		
		parametroMap.put("rolId", idRol);		
		parametroMap.put("aplicacionId", variablesSistema.aplicacionTalentoId.intValue());
		
		RespBase<RespComboUsuarioPorEntidadRol> response = cuentaEntidadService.listarCuentasPorEntidadPorRol(parametroMap);
		
		return ResponseEntity.ok(response);		
	}
	
}