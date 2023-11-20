package pe.gob.servir.entidad.controller;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import javax.xml.bind.DatatypeConverter;

import org.apache.commons.lang3.StringUtils;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
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
import pe.gob.servir.entidad.adapter.BeanAdapterOrganigrama;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.model.Generico;
import pe.gob.servir.entidad.model.ServidorRectorDTO;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.OrganigramaExcelDTO;
import pe.gob.servir.entidad.request.dto.OrganoExcelDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespBuscarUnidadOrganica;
import pe.gob.servir.entidad.response.RespComboPerByOrganigrama;
import pe.gob.servir.entidad.response.RespComboUnidadOrganica;
import pe.gob.servir.entidad.response.RespListaOrganigrama;
import pe.gob.servir.entidad.response.RespListaOrgano;
import pe.gob.servir.entidad.response.RespObtenerGestionOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerLtaOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerServidorCivil;
import pe.gob.servir.entidad.response.RespObtenerValidaOrganigrama;
import pe.gob.servir.entidad.response.RespObtieneLista2;
import pe.gob.servir.entidad.response.RespOrganigrama;
import pe.gob.servir.entidad.response.RespPaises;
import pe.gob.servir.entidad.response.RespParametro;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.GeneralService;
import pe.gob.servir.entidad.service.GestionService;
import pe.gob.servir.entidad.service.OrganigramaService;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@RestController
@Tag(name = "Organigrama", description = "")
public class OrganigramaController {

	private static final Logger LOGGER = Logger.getLogger(OrganigramaController.class);
	
	@Autowired
	private BeanAdapterOrganigrama adapterOrganingrama;

	@Autowired
	private OrganigramaService organigramaService;

	@Autowired
	private GeneralService generalService;

	@Autowired
	private HttpServletRequest httpServletRequest;

	@Autowired
	private VariablesSistema variablesSistema;

	@Autowired
	private GestionService gestionService;

	@Operation(summary = "Crea un organigrama", description = "Crea un organigrama", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespOrganigrama>> registrarOrganigrama(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqOrganigrama> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespOrganigrama> response = organigramaService.guardarOrganigrama(request, jwt, null);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Registrar un organigrama", description = "Registrar un organigrama", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/registrar" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespOrganigrama>> registrarGestionOrganigrama(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqOrganigrama> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespOrganigrama> response = organigramaService.registrarOrganigrama(request, jwt, null);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Validar Duplicidad en Organigrama", description = Constantes.SUM_OBT_LIST
					+ "Valiad duplicida organigrama", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/validaDuplicidad" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerValidaOrganigrama>> validarDuplicidadGestionOrganigrama(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "descripcion", required = false) String descripcion,
			@RequestParam(value = "sigla", required = false) String sigla,
			@RequestParam(value = "idUO", required = false) Long idUO) {
		RespBase<RespObtenerValidaOrganigrama> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("descripcion", descripcion);
		parametroMap.put("sigla", sigla);
		parametroMap.put("idUO", idUO);
		response = organigramaService.verificarDuplicidadOrganigrama(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Organigrama By filtros", description = Constantes.SUM_OBT_LIST
			+ "organigrama by filtros", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/filter" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerOrganigrama>> buscarOrganigramabyFiltro(@PathVariable String access,
			@RequestParam(value = "texto", required = false) String texto,
			@RequestParam(value = "entidadId", required = false) Long entidadId,
			@RequestParam(value = "tipo", required = false) Long tipo) {
		RespBase<RespObtenerOrganigrama> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("texto", texto);
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("tipo", tipo);
		response = organigramaService.buscarOrganigramaByFilter(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Actualiza un organigrama", description = "Actualiza un organigrama", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/{organigramaId}" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespOrganigrama>> actualizarOrganigrama(@PathVariable String access,
			@PathVariable Long organigramaId, @Valid @RequestBody ReqBase<ReqOrganigrama> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespOrganigrama> response = organigramaService.guardarOrganigrama(request, jwt, organigramaId);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Actualiza una gestion organigrama", description = "Actualiza una Gestion organigrama", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/registrar/{organigramaId}" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespOrganigrama>> modificarGestionOrganigrama(@PathVariable String access,
			@PathVariable Long organigramaId, @Valid @RequestBody ReqBase<ReqOrganigrama> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespOrganigrama> response = organigramaService.registrarOrganigrama(request, jwt, organigramaId);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Inactiva un organigrama", description = "Inactiva un organigrama", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@DeleteMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/{organigramaId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> eliminarOrganigrama(@PathVariable String access,
			@PathVariable Long organigramaId, @RequestParam(value = "estado", required = true) String estado) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<Object> response = organigramaService.eliminarOrganigrama(jwt, organigramaId, estado);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Inactiva una gestion organigrama", description = "Inactiva una Gestion Organigrama", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@DeleteMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/eliminar/{organigramaId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> eliminarGestionOrganigrama(@PathVariable String access,
			@PathVariable Long organigramaId, @RequestParam(value = "estado", required = true) String estado) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<Object> response = organigramaService.eliminarGestionOrganigrama(jwt, organigramaId, estado);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Organigrama por el Tipo", description = Constantes.SUM_OBT_LIST
			+ "organigrama por el Tipo", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/tipo" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerLtaOrganigrama>> buscarOrganigramabyTipo(@PathVariable String access,
			@RequestParam(value = "tipo", required = true) Long tipo,
			@RequestParam(value = "entidadId", required = true) Long entidadId) {
		RespBase<RespObtenerLtaOrganigrama> response;
		response = organigramaService.buscarOrganigramaByTipo(tipo, entidadId);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Combo Unidades Organicas por Entidad", description = Constantes.SUM_OBT_LIST
					+ "unidad organica por Entidad", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/unidadorganica/combo" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespComboUnidadOrganica>> buscarUnidadesOrganicasPorEntidad(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId,
			@RequestParam(value = "uoSupId", required = false) Long uoSupId) {
		RespBase<RespComboUnidadOrganica> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put(Constantes.TIPOORGANOID, tipoOrganoId);
		parametroMap.put("uoSupId", uoSupId);
		response = organigramaService.buscarUnidadesOrganicasPorEntidad(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Organigramas con filtro puestoId", description = Constantes.SUM_OBT_LIST
					+ "Lista de Organigramas con puestoId", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/list/visor" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaOrgano>> buscarOrgaigramaTest(@PathVariable String access,
			@RequestParam(value = "entidadId", required = false) Integer entidadId,
			@RequestParam(value = "personaId", required = false) Integer personaId,
			@RequestParam(value = "organigramaId", required = false) Integer organigramaId,
			@RequestParam(value = "puestoId", required = false) Integer puestoId,
			@RequestParam(value = "estado", required = false) String estado,
			@RequestParam(value = "unidadId", required = false) Integer unidadId) {
		RespBase<RespListaOrgano> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("personaId", personaId);
		parametroMap.put("organigramaId", organigramaId);
		parametroMap.put("puestoId", puestoId);
		parametroMap.put("estado", estado);
		parametroMap.put("unidadId", unidadId);
		response = organigramaService.buscarOrganigramaV2(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Combo Unidad Organica Superior", description = Constantes.SUM_OBT_LIST + "Combo UO Superior", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/unidadorganica/superior" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarUnidadOrganica>> buscarUnidadOrganicaSuperior(@PathVariable String access,
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId) {
		RespBase<RespBuscarUnidadOrganica> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put(Constantes.TIPOORGANOID, tipoOrganoId);
		response = organigramaService.buscarUnidadOrganicaSuperior(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Listar Servidores Civiles", description = Constantes.SUM_OBT_LIST
			+ "servidores civiles", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/servidores/civiles" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerServidorCivil>> buscarServidoresCiviles(@PathVariable String access,
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId,
			@RequestParam(value = "unidadOrganicaSuperiorId", required = false) Long unidadOrganicaSuperiorId,
			@RequestParam(value = "unidadOrganicaId", required = false) Long unidadOrganicaId,
			@RequestParam(value = "regimenLaboralId", required = false) Long regimenLaboralId,
			@RequestParam(value = "tipoDocumentoId", required = false) Long tipoDocumentoId,
			@RequestParam(value = "datosServCivil", required = false) String datosServCivil,
			@RequestParam(value = "numeroDocumento", required = false) String numeroDocumento,
			@RequestParam(value = "estadoId", required = false) Long estadoId) {
		RespBase<RespObtenerServidorCivil> response;
 
		Map<String, Object> parametroMap = new HashMap<>();

		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put(Constantes.TIPOORGANOID, tipoOrganoId);
		parametroMap.put("unidadOrganicaSuperiorId", unidadOrganicaSuperiorId);
		parametroMap.put("unidadOrganicaId", unidadOrganicaId);
		parametroMap.put("regimenLaboralId", regimenLaboralId);
		parametroMap.put("tipoDocumentoId", tipoDocumentoId);
		parametroMap.put("datosServCivil", datosServCivil);
		parametroMap.put("numeroDocumento", numeroDocumento);
		parametroMap.put("estadoId", estadoId);
		
		response = organigramaService.selectServidoresCiviles(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Listar Gestion de Organigramas", description = Constantes.SUM_OBT_LIST
					+ "Gestion de Organigramas", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/gestion/listar" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerGestionOrganigrama>> buscarGestionOrganigramas(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId,
			@RequestParam(value = "unidadOrganicaSuperiorId", required = false) Long unidadOrganicaSuperiorId,
			@RequestParam(value = "unidadOrganicaId", required = false) Long unidadOrganicaId) {
		RespBase<RespObtenerGestionOrganigrama> response;

		Map<String, Object> parametroMap = new HashMap<>();

		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put(Constantes.TIPOORGANOID, tipoOrganoId);
		parametroMap.put("unidadOrganicaSuperiorId", unidadOrganicaSuperiorId);
		parametroMap.put("unidadOrganicaId", unidadOrganicaId);

		response = organigramaService.selectGestionOrganigrama(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Organigramas", description = Constantes.SUM_OBT_LIST
			+ "Lista de Organigramas", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama" }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListaOrganigrama>> buscarOrganigramas(@PathVariable String access,
			@RequestParam(value = "entidadId", required = false) Integer entidadId,
			@RequestParam(value = "personaId", required = false) Integer personaId,
			@RequestParam(value = "organigramaId", required = false) Integer organigramaId,
			@RequestParam(value = "puesto", required = false) String puesto,
			@RequestParam(value = "estado", required = false) String estado,
			@RequestParam(value = "unidadId", required = false) Integer unidadId) {
		RespBase<RespListaOrganigrama> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("personaId", personaId);
		parametroMap.put("organigramaId", organigramaId);
		parametroMap.put("puesto", puesto);
		parametroMap.put("estado", estado);
		parametroMap.put("unidadId", unidadId);
		response = organigramaService.buscarOrganigramas(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Combo Personas By Organigrama", description = Constantes.SUM_OBT_LIST
					+ "Combo Personas By Organigrama", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/combo" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespComboPerByOrganigrama>> buscarOrganigramas(@PathVariable String access,
			@RequestParam(value = "entidadId", required = false) Integer entidadId,
			@RequestParam(value = "nombreApellidos", required = false) String nombreApellidos) {
		RespBase<RespComboPerByOrganigrama> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("nombreApellidos", nombreApellidos);
		response = organigramaService.comboPersonaByOrganigrama(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Organigramas", description = Constantes.SUM_OBT_LIST
			+ "organigramas", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/unidadOrganica" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerOrganigrama>> buscarOrganigrama(@PathVariable String access) {
		RespBase<RespObtenerOrganigrama> response;
		response = organigramaService.buscarOrganigramas();
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "descargar Excel", description = Constantes.SUM_OBT_LIST
			+ "descargar Excel", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/downloadFormat" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> downloadFormat(@PathVariable String access,
			@RequestParam(value = "idEntidad", required = false) Long idEntidad) {
		try {

			String rutaA = variablesSistema.rutaExcelLinuxOrgano;

			try (InputStream uploadedInputStream = new FileInputStream((new File(rutaA)))) {
				Map<String, Object> parametroMap = new HashMap<>();
				parametroMap.put(Constantes.ENTIDADID, idEntidad);
				RespBase<RespParametro> listaEstado = generalService.comboEstado();
				RespBase<RespParametro> listaNivel = generalService
						.comboNivel(String.valueOf(variablesSistema.tipoOrgano));
				RespBase<RespParametro> listaNaturaleza = generalService.comboNaturalezaOrgano();
				RespBase<RespParametro> listaTipoDoc = generalService.comboTipoDocumento();
				RespBase<RespObtenerOrganigrama> listaOrgano = organigramaService
						.buscarOrganigramaByFilter(parametroMap);
				RespBase<RespPaises> listaPaises = generalService.comboPaises();
				listaTipoDoc = ParametrosUtil.listaTipoDNI_CE(listaTipoDoc.getPayload().getListaParametros());

				List<Generico> listaEstadoGen = ParametrosUtil
						.convertirEstadoAGenerico(listaEstado.getPayload().getListaParametros());
				List<Generico> listaNivelGen = ParametrosUtil
						.convertirNivelAGenerico(listaNivel.getPayload().getListaParametros());
				List<Generico> listaNaturalezaGen = ParametrosUtil
						.convertirNaturalezaAGenerico(listaNaturaleza.getPayload().getListaParametros());
				List<Generico> listaTipoDocGen = ParametrosUtil
						.convertirTipoDocumentoAGenerico(listaTipoDoc.getPayload().getListaParametros());
				List<Generico> listaOrganoGen = ParametrosUtil
						.convertirOrganoAGenerico(listaOrgano.getPayload().getListaOrganigrama());
				List<Generico> listaPaisesGen = ParametrosUtil
						.convertirPaisesAGenerico(listaPaises.getPayload().getListaPaises());

				Map<String, Object> mapaLista = new LinkedHashMap<>();
				mapaLista.put("ESTADO", listaEstadoGen);
				mapaLista.put("NIVEL", listaNivelGen);
				mapaLista.put("NATURALEZA", listaNaturalezaGen);
				mapaLista.put("ORGANO", listaOrganoGen);
				mapaLista.put("TIPOS DOCUMENTOS", listaTipoDocGen);
				mapaLista.put("PAISES", listaPaisesGen);

				byte[] arrayExcel = ExcelUtil.updateDropDownXLSX(uploadedInputStream, mapaLista);
				RespBase<Object> response = new RespBase<>();
				response.getStatus().setSuccess(Boolean.TRUE);
				response.setPayload(Base64.getEncoder().encodeToString(arrayExcel));

				return ResponseEntity.ok(response);

			}
		} catch (Exception e) {

			RespBase<Object> response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Operation(summary = "Crea organigrama masivo", description = "Crea organigrama masivo", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/masivo" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> registrarOrganigramaMasivo(@PathVariable String access,
			@RequestBody String tramaJSON) {
		RespBase<Object> response = new RespBase<>();
		ExcelUtil<OrganoExcelDTO> validacion = new ExcelUtil(OrganoExcelDTO::new);
		try {
			MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
			Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(tramaJSON, Map.class);

			String base64 = parametros.get("value").toString();
			Long entidadId = Long.parseLong(parametros.get(Constantes.ENTIDADID).toString());
			byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
			InputStream uploadedInputStream = new ByteArrayInputStream(bytes);
			InputStream uploadedInputStreamObser = new ByteArrayInputStream(bytes);

			List<OrganoExcelDTO> lista = organigramaService.validarCargaMasivaOrganigrama(uploadedInputStream);
			response = organigramaService.cargaMasivaOrganigrama(jwt, lista, entidadId);
			String fileBase64 = validacion.addObservacionesXLSXBase64(uploadedInputStreamObser, lista, true);

			for (int i = lista.size() - 1; i >= 0; i--) {
				if (StringUtils.isEmpty(lista.get(i).getObservacionResultado())) {
					lista.remove(i);
				} else {
					String observacion = "FILA " + (i + 2) + " : " + lista.get(i).getObservacionResultado();
					lista.get(i).setFilaObservacion(observacion);
				}
			}

			response.getStatus().setSuccess(Boolean.TRUE);
			Map<String, Object> devol = new HashMap<>();
			devol.put("archivo", fileBase64);
			devol.put("organo", lista);
			response.setPayload(devol);

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

	// SGM
	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "descargar plantilla Excel Organigrama", description = Constantes.SUM_OBT_LIST
					+ "descargar Excel", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/downloadFormatOrganigrama" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> downloadFormatOrganigrama(@PathVariable String access,
			@RequestParam(name = "idEntidad", required = false) Long idEntidad) {
		try {

			//String pathFileOrg = System.getProperty("user.home") + variablesSistema.rutaExcelLinuxOrganigrama;

			String pathFileOrg = variablesSistema.rutaExcelLinuxOrganigrama;

			LOGGER.info(pathFileOrg);

			try (InputStream uploadedInputStream = new FileInputStream((new File(pathFileOrg)))) {

				Map<String, Object> parametroMap = new HashMap<>();
				parametroMap.put(Constantes.ENTIDADID, idEntidad);

				RespBase<RespParametro> listaOrgano = generalService.comboNaturalezaOrgano();

				List<Generico> listaOrganoGen = ParametrosUtil.convertirNivelAGenerico(listaOrgano.getPayload().getListaParametros());

				Map<String, Object> mapaLista = new LinkedHashMap<>();
				mapaLista.put("TIPO_ORGANO", listaOrganoGen);

				byte[] arrayExcel = ExcelUtil.updateDropDownXLSX(uploadedInputStream, mapaLista);

				RespBase<Object> response = new RespBase<>();

				response.getStatus().setSuccess(Boolean.TRUE);
				response.setPayload(Base64.getEncoder().encodeToString(arrayExcel));

				return ResponseEntity.ok(response);
			}
		} catch (Exception e) {
			RespBase<Object> response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Operation(summary = "Validar datos organigrama llenados en el excel", description = "Agregar observaciones", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/validaFormatoOrganigrama" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> validarFormatoOrganigrama(@PathVariable String access,
			@RequestBody String tramaJSON) {
		RespBase<Object> response = new RespBase<>();
		ExcelUtil<OrganigramaExcelDTO> validacion = new ExcelUtil(OrganigramaExcelDTO::new);
		List<OrganigramaExcelDTO> listaCompletaExcel = new ArrayList<>();
		List<OrganigramaExcelDTO> lista = new ArrayList<>();
		try {
			MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
			Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(tramaJSON, Map.class);

			String base64 = parametros.get("value").toString();
			Long entidadId = Long.parseLong(parametros.get(Constantes.ENTIDADID).toString());
			byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
			byte[] combos = DatatypeConverter.parseBase64Binary(base64);
			InputStream uploadedInputStream = new ByteArrayInputStream(bytes);
			InputStream uploadedInputStreamObserv = new ByteArrayInputStream(bytes);

			lista = organigramaService.validarCargaMasivaOrgExcel(uploadedInputStream);

			if (!CollectionUtils.isEmpty(lista)) {
				listaCompletaExcel = organigramaService.obetenerCodigoCombo(combos, lista);
				response = organigramaService.cargaMasivaOrganigramaSGM(jwt, listaCompletaExcel, entidadId);
			} else {
				OrganigramaExcelDTO errorOrganigrama = new OrganigramaExcelDTO();
				errorOrganigrama.setObservacionResultado(Constantes.NO_SE_INGRESO_DATOS);
				lista.add(errorOrganigrama);
				
				byte[] archivoObservado = adapterOrganingrama.excelObservacionOrganigrama(lista, uploadedInputStreamObserv);
				Map<String, Object> devol = new HashMap<>();
				devol.put("archivo",  Base64.getEncoder().encodeToString(archivoObservado));
				devol.put("organigrama", lista);
				response.setPayload(devol);
				response.getStatus().setSuccess(false);
				return ResponseEntity.ok(response);
			}
			if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
				String fileBase64 = validacion.addObservacionesXLSXBase64(uploadedInputStreamObserv, listaCompletaExcel, false);

				for (int i = listaCompletaExcel.size() - 1; i >= 0; i--) {
					if (StringUtils.isEmpty(listaCompletaExcel.get(i).getObservacionResultado())) {
						listaCompletaExcel.remove(i);
					} else {
						String observacion = "FILA " + (i + 2) + " : " + listaCompletaExcel.get(i).getObservacionResultado();
						lista.get(i).setFilaObservacion(observacion);
					}
				}

				Map<String, Object> devol = new HashMap<>();
				devol.put("archivo", fileBase64);
				devol.put("organigrama", lista);
				response.setPayload(devol);
			} else {
				String archivo = "";
				Map<String, Object> devol = new HashMap<>();
				devol.put("archivo", archivo);
				devol.put("organigrama", listaCompletaExcel);
				response.setPayload(devol);
			}
			return ResponseEntity.ok(response);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Listar Servidores Rectores", description = Constantes.SUM_OBT_LIST
			+ "servidores rectores", tags = { "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/organigrama/servidores/rectores" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneLista2<ServidorRectorDTO>>> buscarServidoresRectores(
			@PathVariable String access, @RequestParam(value = "entidadId") Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId,
			@RequestParam(value = "unidadOrganicaSuperiorId", required = false) Long unidadOrganicaSuperiorId,
			@RequestParam(value = "unidadOrganicaId", required = false) Long unidadOrganicaId,
			@RequestParam(value = "regimenLaboralId", required = false) Long regimenLaboralId,
			@RequestParam(value = "tipoDocumentoId", required = false) Long tipoDocumentoId,
			@RequestParam(value = "datosServCivil", required = false) String datosServCivil,
			@RequestParam(value = "numeroDocumento", required = false) String numeroDocumento) {

		RespBase<RespObtieneLista2<ServidorRectorDTO>> response = this.gestionService.listarServidoresRectores(
				entidadId, tipoOrganoId, unidadOrganicaSuperiorId, unidadOrganicaId, regimenLaboralId, tipoDocumentoId,
				datosServCivil, numeroDocumento);
		return ResponseEntity.ok(response);
	}

}
