package pe.gob.servir.entidad.controller;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.GestorDTO;
import pe.gob.servir.entidad.request.ReqActualizaGestorORH;
import pe.gob.servir.entidad.request.ReqGestorORH;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespGestores;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.GestoresService;

@RestController
@Tag(name= "Gestores", description = "")
public class GestoresController {

	@Autowired
	private GestoresService gestoresService;
	
	@Autowired
	private HttpServletRequest httpServletRequest;
	
	@Operation(summary = "Listar gestores ORH", description = "Listar gestores ORH", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/gestores/orh/{entidadId}" }, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> ObtieneSolicitudesExternasEntidad(
			@PathVariable String access, @PathVariable Long entidadId
			) {
		RespBase<Object> response = gestoresService.listaGestoresOrh(entidadId);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Listar gestores ORH", description = "Listar gestores ORH", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT+"/gestores/orh" }, 
			 	consumes = {MediaType.APPLICATION_JSON_VALUE },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespGestores>> registraGestor(
			@PathVariable String access
			 , @Valid @RequestBody ReqGestorORH request
			) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespGestores> response = gestoresService.registrarGestores(request, jwt);
		return ResponseEntity.ok(response);

	}
	
	@Operation(summary = "Obtener gestor ORH ID", description = "Obtener gestor ORH ID", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/gestores/orhById/{gestorId}" }, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<GestorDTO>> getGestorId(
			@PathVariable String access, 
			@PathVariable Long gestorId) {
		RespBase<GestorDTO> response = gestoresService.getGestorId(gestorId);
		return ResponseEntity.ok(response);

	}
	
	@Operation(summary = "Actualizar gestores ORH", description = "Actualizar gestores ORH", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/gestores/orh" }, 
			 	consumes = {MediaType.APPLICATION_JSON_VALUE },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespGestores>> ActualizaGestor(
			@PathVariable String access
			 , @Valid @RequestBody ReqActualizaGestorORH request
			) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespGestores> response = gestoresService.actualizaGestores(request, jwt);
		return ResponseEntity.ok(response);

	}

}
