package pe.gob.servir.entidad.controller;

import java.util.HashMap;
import java.util.Map;


import javax.validation.constraints.NotNull;

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
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqSede;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerSede;
import pe.gob.servir.entidad.response.RespSede;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.SedeService;

@RestController
@Tag(name = "Sede", description = "")
public class SedeController {
	
	@Autowired
	private HttpServletRequest httpServletRequest;
	
	@Autowired
	SedeService sedeService;
	
	@Operation(summary = Constantes.SUM_OBT_LIST + "Sede By filtros", description = Constantes.SUM_OBT_LIST
			+ "Sede by filtros", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/sede/filter" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerSede>> buscarSedeByFiltro(@PathVariable String access,			
			@RequestParam(value = "entidadId", required = false)@NotNull(message = "entidad no puede ser nulo") @Valid Integer entidadId,
			@RequestParam(value = "distrito", required = false) Integer distrito,
			@RequestParam(value = "provincia", required = false) Integer provincia,
			@RequestParam(value = "departamento", required = false) Integer departamento,
			@RequestParam(value = "estado", required = false) String estado,
			@RequestParam(value = "sede", required = false) Integer sede) {
		RespBase<RespObtenerSede> response = null;
		
		Map<String, Object> parametroMap = new HashMap<>();		
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("distrito", distrito);
		parametroMap.put("provincia", provincia);
		parametroMap.put("departamento", departamento);
		parametroMap.put("estado", estado);
		parametroMap.put("sedeId", sede);
		response = sedeService.buscarSedeByFilter(parametroMap);
		
		return ResponseEntity.ok(response);
	}

	
	@Operation(summary = "Crea una sede", description = "Crea una sede", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class))
			}),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class))
			}) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/sede" }, consumes = { 
				MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespSede>> registrarSede(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqSede> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespSede> response = sedeService.guardarSede(request, jwt, null);
		return ResponseEntity.ok(response);
		}
	

	@Operation(summary = "Actualiza una sede", description = "Actualiza una sede", tags = { "" },security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class))
			}),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class))
			}) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/sede/{sedeId}" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespSede>> actualizarSede(@PathVariable String access,
			@PathVariable Long sedeId, @Valid @RequestBody ReqBase<ReqSede> request){
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespSede> response = sedeService.guardarSede(request, jwt, sedeId);
		return ResponseEntity.ok(response);	
	
	}
	
	@Operation(summary = "Inactiva una sede", description = "Inactiva una sede", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@DeleteMapping(path = { Constantes.BASE_ENDPOINT + "/sede/{sedeId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> eliminarSede(@PathVariable String access,
			@PathVariable Long sedeId,
			@RequestParam(value = "estado", required = true) String estado){
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<Object> response = sedeService.eliminarSede(jwt, sedeId, estado);
		return ResponseEntity.ok(response);
	}
	
}
