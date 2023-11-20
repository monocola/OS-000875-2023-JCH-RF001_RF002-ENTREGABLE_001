package pe.gob.servir.entidad.controller;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
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
import pe.gob.servir.entidad.request.ReqEntidad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespEntidad;
import pe.gob.servir.entidad.response.RespListaEntidad;
import pe.gob.servir.entidad.response.RespListaServidoresCiviles;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGDR;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGDRGraficosDonats;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGraficosDonats;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.EntidadService;
import pe.gob.servir.entidad.util.ParametrosUtil;
@RestController
@Tag(name = "Entidad", description = "")
public class EntidadController {
	
	@Autowired
	private HttpServletRequest httpServletRequest;
	
	@Autowired
	private EntidadService entidadService;
	
	@Operation(summary = "Actualizacion de los datos de la entidad", description = "Actualiza Datos de la entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/{entidadId}"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespEntidad>> actualizarEntidad(
			@PathVariable String access,
			@PathVariable Long entidadId,
			@Valid @RequestBody ReqBase<ReqEntidad> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<RespEntidad> response = entidadService.actualizarEntidad(request, jwt,entidadId);
		if(!response.getStatus().getSuccess()) {
			response = ParametrosUtil.setearResponse(response,Boolean.FALSE,Constantes.RESPONSE_MESSAGE);
		}
		return ResponseEntity.ok(response);		
	}
	
	@Operation(summary = "Actualizacion de los datos de la entidad desde GME", description = "Actualiza Datos de la entidad desde GME", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/actualizar/entidad/{entidadId}"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespEntidad>> actualizarEntidadGME(
			@PathVariable String access,
			@PathVariable Long entidadId,
			@Valid @RequestBody ReqBase<ReqEntidad> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<RespEntidad> response = entidadService.actualizarEntidadGme(request, jwt,entidadId);
		if(!response.getStatus().getSuccess()) {
			response = ParametrosUtil.setearResponse(response,Boolean.FALSE,Constantes.RESPONSE_MESSAGE);
		}
		return ResponseEntity.ok(response);		
	}

	@Operation(summary = "Obtiene una entidad", description = "Obtiene una entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/{entidadId}" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaEntidad>> listarEntidad(
			@PathVariable String access,
			@PathVariable Long entidadId) {
		RespBase<RespListaEntidad> response = entidadService.listarEntidad(entidadId);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Listar las entidades activas", description = "Listar las entidades activas", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaEntidad>> listarEntidades(
			@PathVariable String access) {
		RespBase<RespListaEntidad> response = entidadService.listarEntidades();
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Listar resumen servidores civiles", description = "Listar resumen servidores civiles", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/resumen-servidores-civiles/{entidadId}" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaServidoresCiviles>> resumenServidoresCiviles(
			@PathVariable String access, @PathVariable Long entidadId) {
		RespBase<RespListaServidoresCiviles> response = entidadService.resumenDeServidoresCiviles(entidadId);
		return ResponseEntity.ok(response);
	} 
	
	@Operation(summary = "Listar resumen servidores civiles GDR", description = "Listar resumen servidores civiles GDR", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/resumen-servidores-civiles-gdr/{entidadId}" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaServidoresCivilesGDR>> resumenServidoresCivilesGDR(
			@PathVariable String access, @PathVariable Long entidadId) {
		RespBase<RespListaServidoresCivilesGDR> response = entidadService.resumenDeServidoresCivilesGDR(entidadId);
		return ResponseEntity.ok(response);
	} 
	
	@Operation(summary = "Listar resumen servidores civiles segun tipo de organo", description = "Listar resumen servidores civiles SegunTipoOrgano", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/gme/resumen-servidores-civiles/graficos-donats/{entidadId}" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaServidoresCivilesGraficosDonats>> resumenServidoresCivilesGraficoDonats(
			@PathVariable String access, @PathVariable Long entidadId) {
		RespBase<RespListaServidoresCivilesGraficosDonats> response = entidadService.resumenDeServidoresCivilesGraficosDonats(entidadId);		
		return ResponseEntity.ok(response);
	} 
	
	@Operation(summary = "Listar resumen servidores civiles segun tipo de organo", description = "Listar resumen servidores civiles SegunTipoOrgano", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/gme/resumen-servidores-civiles-gdr/graficos-donats/{entidadId}" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaServidoresCivilesGDRGraficosDonats>> resumenServidoresCivilesGDRGraficoDonats(
			@PathVariable String access, @PathVariable Long entidadId) {
		RespBase<RespListaServidoresCivilesGDRGraficosDonats> response = entidadService.resumenDeServidoresCivilesGDRGraficosDonats(entidadId);		
		return ResponseEntity.ok(response);
	} 
	
	@Operation(summary = "Listar las entidades filtradas por un grupo de idEntidad", description = "Listar las entidades filtradas por un grupo de idEntidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/filter/" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaEntidad>> listarEntidadesPorIds(
			@PathVariable String access,
			@RequestParam(value = "listId", required = true) String listId) {
		RespBase<RespListaEntidad> response = entidadService.listarEntidadesPorListId(listId);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Obtiene una entidad por filtros", description = "Obtiene una entidad por filtros", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/entidad/filt" }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaEntidad>> buscarEntidadFilter(@PathVariable String access,
			@RequestParam(value = "entidadId", required = false) Long entidadId,
			@RequestParam(value = "sectorId", required = false) Long sectorId,
			@RequestParam(value = "nivelGobiernoId", required = false) Long nivelGobiernoId,
			@RequestParam(value = "tipoEntidadId", required = false) Long tipoEntidadId,
			@RequestParam(value = "tipoEntidadPubId", required = false) Long tipoEntidadPubId,
			@RequestParam(value = "descripcion", required = false) String descripcion) {
		RespBase<RespListaEntidad> response = null;

		Map<String, Object> parametroMap = new HashMap<>();

		parametroMap.put(Constantes.ENTIDADID, entidadId);

		parametroMap.put("sectorId", sectorId);

		parametroMap.put("nivelGobiernoId", nivelGobiernoId);

		parametroMap.put("tipoEntidadId", tipoEntidadId);
		
		parametroMap.put("tipoEntidadPubId", tipoEntidadPubId);
		
		parametroMap.put("descripcion", descripcion);

		response = entidadService.listarEntidadFilter(parametroMap);

		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Listar las entidades activas por sigla", description = "Listar las entidades activas por sigla", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/by/" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaEntidad>> listarEntidadesPorSigla(
			@PathVariable String access,
			@RequestParam(value = "sigla", required = true) String sigla) {
		RespBase<RespListaEntidad> response = entidadService.listarEntidadPorSigla(sigla, true);
		return ResponseEntity.ok(response);
	}
	
}
