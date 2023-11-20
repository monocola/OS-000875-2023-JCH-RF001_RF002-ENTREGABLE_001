package pe.gob.servir.entidad.controller;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import javax.xml.bind.DatatypeConverter;

import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqRegistrarPuesto;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespComboPuesto;
import pe.gob.servir.entidad.response.RespListarDetalleUO;
import pe.gob.servir.entidad.response.RespListarPuesto;
import pe.gob.servir.entidad.response.RespPuesto;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.PuestoService;
import pe.gob.servir.entidad.util.JsonUtil;

@RestController
@Tag(name = "Puesto", description = "")
public class PuestoController {

	private static final Logger LOGGER = Logger.getLogger(PuestoController.class);

	@Autowired
	private HttpServletRequest httpServletRequest;

	@Autowired
	private PuestoService puestoService;


	@Operation(summary = Constantes.SUM_OBT_LIST + "Combo Puesto", description = Constantes.SUM_OBT_LIST
			+ "combo puesto", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/combo" }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespComboPuesto>> buscarPuestosPorEntidad(@PathVariable String access,
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "organigramaId", required = true) Long organigramaId) {
		RespBase<RespComboPuesto> response;
		response = puestoService.buscarPuestoPorEntidad(entidadId, organigramaId);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Filtrar Puestos por Descripcion", description = Constantes.SUM_OBT_LIST
					+ "Filtrar puestos por descripcion", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/filtrar" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespComboPuesto>> filtrarPuestos(@PathVariable String access,
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "descripcion", required = false) String descripcion,
			@RequestParam(value = "organigramaId", required = true) Long organigramaId) {
		LOGGER.info("Metodo filtrarPuestos");
		RespBase<RespComboPuesto> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("descripcion", descripcion);
		parametroMap.put("organigramaId", organigramaId);
		response = puestoService.filtrarPuestosPorDescripcion(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "descargar Excel plantilla Puestos", description = Constantes.SUM_OBT_LIST
			+ "descargar Excel", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/downloadFormatPuesto" }, produces = {
			MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<RespBase<Object>> downloadFormatServidorCivil(@PathVariable String access,
			@RequestParam(name = "idEntidad", required = true) Long idEntidad) {
		RespBase<Object> response;
		response = puestoService.descargarExcelPuesto(idEntidad);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Registrar puesto", description = "Registrar puesto", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/registrar" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespPuesto>> registrarPuesto(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqRegistrarPuesto> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespPuesto> response = puestoService.registrarPuesto(request, jwt);
		return ResponseEntity.ok(response);
	}

	@SuppressWarnings({ "unchecked"})
	@Operation(summary = "Validar datos Puestos llenados en el excel", description = "Agregar observaciones", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/validaFormatoPuesto" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> validarFormatoServidorCivil(@PathVariable String access,
			@RequestBody String tramaJSON) {
		RespBase<Object> response = new RespBase<>();
		try {
			MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
			Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(tramaJSON, Map.class);

			String base64 = parametros.get("value").toString();
			Long entidadId = Long.parseLong(parametros.get(Constantes.ENTIDADID).toString());

			byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
			InputStream uploadedInputStream = new ByteArrayInputStream(bytes);
			response = puestoService.obtenerListaPuestosfromExcel(uploadedInputStream, entidadId, jwt);

			return ResponseEntity.ok(response);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);

			response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}

	@Operation(summary = "Obteber puesto", description = "Obteber puesto", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/listar" }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListarPuesto>> listarPuesto(@PathVariable String access,
			@RequestParam(value = "esJefe", required = true) String esJefe,
			@RequestParam(value = "unidadOrganicaID", required = true) Long unidadOrganicaID,
			@RequestParam(value = "nombrePuesto", required = true) String nombrePuesto,
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "puestoId", required = true) Long puestoId) throws ValidationException {
		RespBase<RespListarPuesto> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("esJefe", esJefe);
		parametroMap.put("unidadOrganicaID", unidadOrganicaID);
		parametroMap.put("nombrePuesto", nombrePuesto);
		parametroMap.put("entidadId", entidadId);
		parametroMap.put("puestoId", puestoId);
		response = puestoService.listarPuesto(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Editar Puesto", description = "Editar Puesto", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/editar/{puestoId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> editarPuesto(@PathVariable String access, @PathVariable Long puestoId,
			@Valid @RequestBody ReqBase<ReqRegistrarPuesto> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<Object> response = puestoService.editarPuesto(request, jwt, puestoId);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Obteber lista puestos UO por evaluador", description = "Obteber lista puestos UO por evaluador", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/listaUOxEvaluador" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListarDetalleUO>> listarPuestosUOxEvaluador(@PathVariable String access,
			@RequestParam(value = "personaId", required = true) Long personaId,
			@RequestParam(value = "entidadId", required = true) Long entidadId) throws ValidationException {
		RespBase<RespListarDetalleUO> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("personaId", personaId);
		parametroMap.put("entidadId", entidadId);
		response = puestoService.listarPuestosUOxEvaluador(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Obteber lista puestos UO por evaluador desde el evaluado", description = "Obteber lista puestos UO por evaluador desde el evaluado", tags = {
		"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/puesto/listaUOxEvaluadorPorEvaluado" }, produces = {
		MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListarDetalleUO>> listarPuestosUOxEvaluadoGDR(@PathVariable String access,
		@RequestParam(value = "personaId", required = true) Long personaId,
		@RequestParam(value = "entidadId", required = true) Long entidadId) throws ValidationException {
	RespBase<RespListarDetalleUO> response;
	Map<String, Object> parametroMap = new HashMap<>();
	parametroMap.put("personaId", personaId);
	parametroMap.put("entidadId", entidadId);
	response = puestoService.listarPuestosUOxEvaluadorGDR(parametroMap);
	return ResponseEntity.ok(response);
	}
}
