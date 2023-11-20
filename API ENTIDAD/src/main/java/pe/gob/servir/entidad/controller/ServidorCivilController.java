package pe.gob.servir.entidad.controller;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
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

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import feign.FeignException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.model.DetUnidadOrganica;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.Generico;
import pe.gob.servir.entidad.request.ReqActualizaNuevoEvaluadorServidorCivil;
import pe.gob.servir.entidad.request.ReqActualizarPuesto;
import pe.gob.servir.entidad.request.ReqAgregarPuesto;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEditaServidorCivil;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.EditParticipanteGDRDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilExcelDTO;
import pe.gob.servir.entidad.response.*;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.*;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@RestController
@Tag(name = "ServidorCivil", description = "")
public class ServidorCivilController {

	private static final Logger LOGGER = Logger.getLogger(ServidorCivilController.class);

	@Autowired
	private BeanAdapterServidorCivil adapterServidorCivil;
	
	@Autowired
	private OrganigramaService organigramaService;

	@Autowired
	ServidorCivilService servidorCivilService;

	@Autowired
	private PuestoService puestoService;

	@Autowired
	private GeneralService generalService;

	@Autowired
	private VariablesSistema variablesSistema;

	@Autowired
	private HttpServletRequest httpServletRequest;

	@Autowired
	private GestionService gestionService;

	@Operation(summary = "Crea un Servidor Civil", description = "Crea un servidor civil", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/crear" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<GenericResponseMessage>> registrarServidorCivil(@PathVariable String access,
			@Valid @RequestBody ReqBase<BeanServidorCivilDTO> request) throws Exception {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<GenericResponseMessage> response = servidorCivilService.crearServidorCivil(request, jwt);
		return ResponseEntity.ok(response);

	}

	@Operation(summary = "Cesar Servidor Civil", description = "Cesar Servidor Civil", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/cesar" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> cesarServidorCivil(@PathVariable String access,
			@Valid @RequestBody ReqBase<DetUnidadOrganica> request) {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response = servidorCivilService.cesarServidorCivil(request, jwt);

		return ResponseEntity.ok(response);

	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Obtener Datos Personales Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Obtener datos personales", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/datosPersonales" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerDatosPersonalesServidorCivilDTO>> obtenerDatosPersonalesServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "detuoId", required = false) Long detuoId,
			@RequestParam(value = "personaId", required = false) Long personaId,
			@RequestParam(value = "regimenId", required = false) Long regimenId) throws ValidationException {
		RespBase<RespObtenerDatosPersonalesServidorCivilDTO> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("detuoId", detuoId);
		parametroMap.put(Constantes.PERSONAID, personaId);
		parametroMap.put("regimenId", regimenId);
		response = servidorCivilService.obtenerDatosPersonalesServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Obtener Puesto de UO en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Obtener puesto uo servidor civil", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/puestoUO" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenePuestoUoServidorCivil>> obtenerPuestoUoServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = false) Long uoId,
			@RequestParam(value = "personaId", required = false) Long personaId) throws ValidationException {
		RespBase<RespObtenePuestoUoServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put(Constantes.PERSONAID, personaId);
		response = servidorCivilService.obtenerPuestoUoServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Inactiva un Servidor Civil", description = "Inactiva un Servidor Civil", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })

	@DeleteMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/{detUnidadOrganicaId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> eliminarServidorCivil(@PathVariable String access,
			@PathVariable Long detUnidadOrganicaId, @RequestParam(value = "estado", required = true) String estado) {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response = servidorCivilService.eliminarServidorCivil(jwt, detUnidadOrganicaId, estado);

		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Personas por Puesto y UO en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar personas puesto uo servidor civil", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/personasPuesto" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarPersonasPuestoUoServidorCivil>> buscarPersonasPuestoUoServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = false) Long uoId,
			@RequestParam(value = "puestoId", required = false) Long puestoId) throws ValidationException {
		RespBase<RespBuscarPersonasPuestoUoServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put("puestoId", puestoId);
		response = servidorCivilService.buscarPersonasPuestoUoServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar participantes en servidor civil", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteServidorCivil>> buscarParticipantesServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId,
			@RequestParam(value = "unidadOrganicaSuperiorId", required = false) Long unidadOrganicaSuperiorId,
			@RequestParam(value = "unidadOrganicaId", required = false) Long unidadOrganicaId,
			@RequestParam(value = "regimenLaboralId", required = false) Long regimenLaboralId,
			@RequestParam(value = "tipoDocumentoId", required = false) Long tipoDocumentoId,
			@RequestParam(value = "datosServCivil", required = false) String datosServCivil,
			@RequestParam(value = "numeroDocumento", required = false) String numeroDocumento,
			@RequestParam(value = "estadoId", required = false) Long estadoId,
			@RequestParam(value = "estadoSerCivGdrId", required = false) Long estadoSerCivGdrId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteServidorCivil> response;

		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("tipoOrganoId", tipoOrganoId);
		parametroMap.put("unidadOrganicaSuperiorId", unidadOrganicaSuperiorId);
		parametroMap.put("unidadOrganicaId", unidadOrganicaId);
		parametroMap.put("regimenLaboralId", regimenLaboralId);
		parametroMap.put("tipoDocumentoId", tipoDocumentoId);
		parametroMap.put("datosServCivil", datosServCivil);
		parametroMap.put("numeroDocumento", numeroDocumento);
		parametroMap.put("estadoId", estadoId);
		parametroMap.put("estadoSerCivGdrId", estadoSerCivGdrId);

		response = servidorCivilService.buscarParticipanteServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}
	 
	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar participantes en servidor civil", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/noActivos" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteServidorCivil>> buscarParticipantesServidorCivilNoActivos(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "tipoOrganoId", required = false) Long tipoOrganoId,
			@RequestParam(value = "unidadOrganicaSuperiorId", required = false) Long unidadOrganicaSuperiorId,
			@RequestParam(value = "unidadOrganicaId", required = false) Long unidadOrganicaId,
			@RequestParam(value = "regimenLaboralId", required = false) Long regimenLaboralId,
			@RequestParam(value = "tipoDocumentoId", required = false) Long tipoDocumentoId,
			@RequestParam(value = "datosServCivil", required = false) String datosServCivil,
			@RequestParam(value = "numeroDocumento", required = false) String numeroDocumento,
			@RequestParam(value = "estadoId", required = false) Long estadoId,
			@RequestParam(value = "estadoSerCivGdrId", required = false) Long estadoSerCivGdrId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteServidorCivil> response;

		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("tipoOrganoId", tipoOrganoId);
		parametroMap.put("unidadOrganicaSuperiorId", unidadOrganicaSuperiorId);
		parametroMap.put("unidadOrganicaId", unidadOrganicaId);
		parametroMap.put("regimenLaboralId", regimenLaboralId);
		parametroMap.put("tipoDocumentoId", tipoDocumentoId);
		parametroMap.put("datosServCivil", datosServCivil);
		parametroMap.put("numeroDocumento", numeroDocumento);
		parametroMap.put("estadoId", estadoId);
		parametroMap.put("estadoSerCivGdrId", estadoSerCivGdrId);

		response = servidorCivilService.buscarParticipanteServidorCivilNoActivos(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluadores por Rol en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar participantes evaluadores por rol", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluadores" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadoresServidorCivil>> buscarParticipantesEvaluadoresServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadoresServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		response = servidorCivilService.buscarParticipantesEvaluadoresServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluados en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar particiapantes evaluados por un evaluador", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluados" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosServidorCivil>> buscarParticipantesEvaluadosServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = true) Long uoId,
			@RequestParam(value = "personaEvaluadorId", required = true) Long personaEvaluadorId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put(Constantes.PERSONAEVALUADORID, personaEvaluadorId);
		response = servidorCivilService.buscarParticipantesEvaluadosServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluados Sin Mando Medio en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar particiapantes evaluados sin mando medio", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = {
			Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluados/noMandoMedio" }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosServidorCivil>> buscarParticipantesEvaluadosNoMandoMedioServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = true) Long uoId,
			@RequestParam(value = "personaEvaluadorId", required = false) Long personaEvaluadorId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put(Constantes.PERSONAEVALUADORID, personaEvaluadorId);
		response = servidorCivilService.buscarParticipantesEvaluadosNoMandoMedioServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluados Sin Evaluador Mando Medio en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar particiapantes evaluados sin evaluador por un evaluador", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = {
			Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluados/sinEvaluadorMandoMedio" }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosServidorCivil>> buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "detUoId", required = true) Long detUoId,
			@RequestParam(value = "uoId", required = true) Long uoId,
			@RequestParam(value = "personaEvaluadorId", required = false) Long personaEvaluadorId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("detUoId", detUoId);
		parametroMap.put("uoId", uoId);
		parametroMap.put(Constantes.PERSONAEVALUADORID, personaEvaluadorId);
		response = servidorCivilService.buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Listar Participantes con Mando Medio en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Listar particiapantes con mando medio", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/mandoMedio" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosServidorCivil>> buscarParticipantesMandoMedioServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = true) Long uoId,
			@RequestParam(value = "personaEvaluadorId", required = true) Long personaEvaluadorId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put(Constantes.PERSONAEVALUADORID, personaEvaluadorId);
		response = servidorCivilService.buscarParticipantesMandoMedioServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Listar Evaluados sin Evluadores en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Listar evaluados sin evaluadores", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = {
			Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluadosSinEvaluadores" }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil>> buscarEvaluadosSinEvaluadoresServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = true) Long uoId) throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		response = servidorCivilService.buscarEvaluadosSinEvaluadoresServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Actualizar Participantes Nuevo Evaluador Servidor Civil", description = "actualizar participantes con su nuevo evaluador Servidor Civil", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = {
			Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/actualizar/nuevoEvaluador" }, consumes = {
					MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> actualizarParticiapantesNuevoEvaluadorServidorCivil(
			@PathVariable String access, @Valid @RequestBody ReqBase<ReqActualizaNuevoEvaluadorServidorCivil> request)
			throws ValidationException {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response = servidorCivilService.actualizarParticiapantesNuevoEvaluadorServidorCivil(request,
				jwt);

		return ResponseEntity.ok(response);

	}

	@Operation(summary = "Actualizar Participantes Nuevo Evaluador Servidor Civil", description = "actualizar participantes con su nuevo evaluador Servidor Civil", tags = {
	"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
@ApiResponses(value = {
	@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
	@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
			@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
	@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
			@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
@PutMapping(path = {
	Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/actualizar/Evaluador" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
public ResponseEntity<RespBase<Object>> actualizarParticiapantesEvaluadorServidorCivil(
	@PathVariable String access, @Valid @RequestBody ReqBase<ReqActualizaNuevoEvaluadorServidorCivil> request)
	throws ValidationException {

MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

RespBase<Object> response = servidorCivilService.actualizarParticiapantesEvaluadorServidorCivil(request,
		jwt);

return ResponseEntity.ok(response);

}
	
	@Operation(summary = "Quitar Un Evaluador de Mando Medio en Servidor Civil", description = "quitar un evaluador de mando medio Servidor Civil", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@DeleteMapping(path = { Constantes.BASE_ENDPOINT
			+ "/servidorCivil/participantes/quitar/evaluadorAsignadoUnEvaluado/{evaluadoDetUoId}" }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> quitarEvaluadorAsignadoUnEvaluadoServidorCivil(@PathVariable String access,
			@PathVariable Long evaluadoDetUoId, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = true) Long uoId) throws ValidationException {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put("evaluadoDetUoId", evaluadoDetUoId);

		RespBase<Object> response = servidorCivilService.quitarEvaluadorAsignadoUnEvaluadoServidorCivil(parametroMap,
				jwt);

		return ResponseEntity.ok(response);

	}

	@Operation(summary = "editar Servidor Civil", description = "editar Servidor Civil", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/editar" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> editarServidorCivil(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqEditaServidorCivil> request) throws ValidationException {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response = servidorCivilService.editarServidorCivil(request, jwt);

		return ResponseEntity.ok(response);

	}

	@Operation(summary = "Agregar Puesto Servidor Civil", description = "Agregar Puesto Cesar Servidor Civil", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/agregarPuesto" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> agregarPuesto(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqAgregarPuesto> request) throws ValidationException {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response = servidorCivilService.agregarPuesto(request, jwt);

		return ResponseEntity.ok(response);

	}

	@Operation(summary = "Actualizar Detalle Puesto Servidor Civil", description = "Actualizar Detalle Puesto Cesar Servidor Civil", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/actualizarDetallePuesto" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> actualizarDetallePuesto(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqActualizarPuesto> request) throws Exception {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response = servidorCivilService.actualizarDetallePuesto(request, jwt);

		return ResponseEntity.ok(response);

	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "descargar Excel plantilla Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "descargar Excel", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/downloadFormatServCivil" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> downloadFormatServidorCivil(@PathVariable String access,
			@RequestParam(name = "idEntidad", required = false) Long idEntidad) {
		try {

			String pathFileOrg = variablesSistema.rutaExcelLinuxServidorCivil;
			// String pathFileOrg = System.getProperty("user.home")+variablesSistema.rutaExcelWindowServidorCivil;

			LOGGER.info(pathFileOrg);

			try (InputStream uploadedInputStream = new FileInputStream((new File(pathFileOrg)))) {

				Map<String, Object> parametroMap = new HashMap<>();
				parametroMap.put(Constantes.ENTIDADID, idEntidad);

				RespBase<RespParametro> listaTipoDoc = generalService.comboTipoDocumento();
				RespBase<RespParametro> listaSexo = generalService.comboParametro(null, Constantes.PARAMETRO_SEXO,
						null);
				RespBase<RespParametro> listaRegLab = generalService.comboParametro(null,
						Constantes.TIPO_REGIMEN_LABORAL, null);
				RespBase<RespParametro> listaSindicato = generalService.comboParametro(null, Constantes.PER_SINDICATO,
						null);

				RespBase<RespComboUnidadOrganica> listaUnidOrganicas = organigramaService
						.buscarUnidadesOrganicasPorEntidad(parametroMap);

				RespBase<RespComboPuesto> listaPuestos = puestoService.buscarPuestoPorEntidad(idEntidad, null);

				listaTipoDoc = ParametrosUtil.listaTipoDNI_CE(listaTipoDoc.getPayload().getListaParametros());

				List<Generico> listTipoDocGen = ParametrosUtil
						.convertirTipoDocumentoAGenerico(listaTipoDoc.getPayload().getListaParametros());
				List<Generico> listTipoSexo = ParametrosUtil
						.convertirEstadoAGenerico(listaSexo.getPayload().getListaParametros());
				List<Generico> listRegimenLab = ParametrosUtil
						.convertirNivelAGenericoRegimenLaboral(listaRegLab.getPayload().getListaParametros());
				List<Generico> listUnidOrganica = ParametrosUtil
						.convertirUnidadOrgAGenerico(listaUnidOrganicas.getPayload().getListaComboUnidadOrganica());
				List<Generico> listSindicato = ParametrosUtil
						.convertirEstadoAGenerico(listaSindicato.getPayload().getListaParametros());

				Map<String, Object> mapaLista = new LinkedHashMap<>();
				mapaLista.put("TIPO DOCUMENTO", listTipoDocGen);
				mapaLista.put("SEXO", listTipoSexo);
				mapaLista.put("REGIMEN LABORAL", listRegimenLab);
				mapaLista.put("SINDICATO", listSindicato);

				Map<String, Object> mapaListaDependientes = new LinkedHashMap<>();
				mapaListaDependientes.put("SIGLA UO", listUnidOrganica);
				mapaListaDependientes.put("PUESTO", listaPuestos.getPayload().getListaComboPuesto());

				byte[] arrayExcel = ExcelUtil.updateCodigoDescrDropDownXLSX(uploadedInputStream, mapaLista,
						mapaListaDependientes);

				RespBase<Object> response = new RespBase<>();

				response.getStatus().setSuccess(Boolean.TRUE);
				response.setPayload(Base64.getEncoder().encodeToString(arrayExcel));

				return ResponseEntity.ok(response);
			} catch (FileNotFoundException e) {
				RespBase<Object> response = new RespBase<>();
				response.getStatus().setSuccess(Boolean.FALSE);
				response.getStatus().getError().getMessages().add("plantilla excel no encontrado " + pathFileOrg);
				LOGGER.error(e.getMessage(), e.getCause());
				HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
				return ResponseEntity.status(status).body(response);
			} catch (IOException e) {
				RespBase<Object> response = new RespBase<>();
				response.getStatus().setSuccess(Boolean.FALSE);
				response.getStatus().getError().getMessages().add("Ha ocurrido un error al procesar el formato.");
				LOGGER.error(e.getMessage(), e.getCause());
				HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
				return ResponseEntity.status(status).body(response);
			} catch (FeignException e) {
				LOGGER.info("Ha ocurrido un error al invocar el Api Entidad " + e.getMessage());
				RespBase<Object> response = new RespBase<>();
				response.getStatus().setSuccess(Boolean.FALSE);
				response.getStatus().getError().getMessages().add("Ha ocurrido un error al invocar el Api Entidad " + e.getMessage());
				HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
				return ResponseEntity.status(status).body(response);
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
	@Operation(summary = "Validar datos servidores civiles llenados en el excel", description = "Agregar observaciones", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/validaFormatoServCivil" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
    @Transactional(timeout = 120000)
	public ResponseEntity<RespBase<Object>> validarFormatoServidorCivil(@PathVariable String access,
			@RequestBody String tramaJSON) {
		RespBase<Object> response = new RespBase<>();
		ExcelUtil<ServidorCivilExcelDTO> validacion = new ExcelUtil(ServidorCivilExcelDTO::new);
       
		try {
			///System.out.println("AQUIIII11111111111111111");
			MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
			Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(tramaJSON, Map.class);
 
			String base64 = parametros.get("value").toString();
			Long entidadId = Long.parseLong(parametros.get(Constantes.ENTIDADID).toString());

			LOGGER.info(">>>>> INICIO del proceso de Carga Masiva de Servidores Civiles. EntidadId = " + entidadId);

			byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
			InputStream uploadedInputStream = new ByteArrayInputStream(bytes);
			InputStream uploadedInputStreamObserv = new ByteArrayInputStream(bytes);

			List<ServidorCivilExcelDTO> lista = servidorCivilService.obtenerListaServCivilfromExcel(uploadedInputStream);
			if (CollectionUtils.isEmpty(lista)) {
				///System.out.println("AQUIIII222222222222222");
				ServidorCivilExcelDTO beanServidor = new ServidorCivilExcelDTO();
				beanServidor.setObservacionResultado(Constantes.NO_SE_INGRESO_DATOS);
				if (lista == null) {
					lista = new ArrayList<>();
				}
				lista.add(beanServidor);
				byte[] archivoObservado = adapterServidorCivil.excelObservacionServidorCivil(lista, uploadedInputStreamObserv);
				Map<String, Object> devol = new HashMap<>();
				devol.put("archivo",  Base64.getEncoder().encodeToString(archivoObservado));
				devol.put("servidorCivil", lista);
				response.setPayload(devol);
				response.getStatus().setSuccess(Boolean.TRUE);
				return ResponseEntity.ok(response);
			} else {
				///System.out.println("AQUIIII333333333333");
				response = servidorCivilService.validarListaServCivil(jwt, lista, entidadId);
				
				if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
					LOGGER.error(">>>>> ERROR en el proceso de Carga Masiva de Servidores Civiles. EntidadId = " + entidadId);
					LOGGER.error(response.getStatus().getError().getMessages());
					///System.out.println("AQUIIII4444444444444444");
					try {
						///System.out.println("AQUIIII5555555555555555");
						String fileBase64 = validacion.addObservacionesXLSXBase64(uploadedInputStreamObserv, lista, true);
						
						for (int i = lista.size() - 1; i >= 0; i--) {
							if (StringUtils.isEmpty(lista.get(i).getObservacionResultado())) {
								lista.remove(i);
							} else {
								String observacion = "FILA " + (i + 2) + " : " + lista.get(i).getObservacionResultado();
								lista.get(i).setFilaObservacion(observacion);
							}
						}
						
						if(fileBase64.equals(Constantes.VACIO)) {
							///System.out.println("AQUIIII666666666666");
							response = new RespBase<>();
							response.getStatus().setSuccess(Boolean.FALSE);
							response.getStatus().getError().getMessages().add("Error al generar archivo de observaciones");
							HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
							
							return ResponseEntity.status(status).body(response);
						} else {
							///System.out.println("AQUIIII77777777777");
							Map<String, Object> devol = new HashMap<>();
							devol.put("archivo", fileBase64);
							devol.put("servidorCivil", lista);
							response.setPayload(devol);
							response.getStatus().setSuccess(Boolean.TRUE);
							return ResponseEntity.ok(response);
						}
					}catch (Exception e) {
						///System.out.println("AQUIIII8888888888");
						response = new RespBase<>();
						response.getStatus().setSuccess(Boolean.FALSE);
						response.getStatus().getError().getMessages().add(e.getMessage());
						HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
						
						return ResponseEntity.status(status).body(response);
					}
				} else {
					 ///System.out.println("AQUIIII9999999999");
					///System.out.println("AQUIIII9999999999"+ response.getStatus().getSuccess());
					LOGGER.info("FIN >>>>> Se realizo la Carga Masiva de Servidores Civiles SATISFACTORIAMENTE. EntidadId = " + entidadId);
					String archivo = "";
					Map<String, Object> devol = new HashMap<>();
					devol.put("archivo", archivo);
					devol.put("servidorCivil", lista);
					response.setPayload(devol);
					return ResponseEntity.ok(response);
				}
			}			
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			 ///System.out.println("AQUIIII1010101010101010");
			response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}

	@Operation(summary = "Actualizar segmento,descripcion y estadoGdr de un participante", description = "Actualizar segmento,descripcion y estadoGdr de un participante", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/editarParticipanteGdr" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> editParticipantesGdr(@PathVariable String access,
			@Valid @RequestBody ReqBase<EditParticipanteGDRDTO> request) throws Exception {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<Object> response = servidorCivilService.editarParticipanteGdr(request, jwt);
		return ResponseEntity.ok(response);

	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluados en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar particiapantes evaluados por un evaluador", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluadosPersona" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosServidorCivil>> buscarParticipantesEvaluadosPersonaServidorCivil(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "personaId", required = true) Long personaId) throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put(Constantes.PERSONAID, personaId);
		response = servidorCivilService.buscarParticipantesEvaluadosPersonaServidorCivil(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Obtiene datos de usuarios", description = "Obtiene datos de usuarios: inputs: detalleUoId, obtienes el id_persona_evaluador con esto su correo", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/obtenerDatosUsuarios" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> obtenerDatosUsuarios(@PathVariable String access,
			@RequestParam(value = "detUoId", required = true) Long detUoId,
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "personaEvaluadorId", required = true) Long personaEvaluadorId) throws ValidationException {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");

		RespBase<Object> response;

		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("detUoId", detUoId);
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("personaEvaluadorId", personaEvaluadorId);

		response = servidorCivilService.obtenerDatosUsuarioEmail(parametroMap, jwt);

		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Obtiene datos de servidor civil, las UOs y puestos activos a las que pertence", description = "Obtiene datos de servidor civil, las UOs y puestos activos a las que pertence", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/uo/puestos" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListarDatosUOXServidorCivil>> obtenerUOsPuestosByServCiv(
			@PathVariable String access, @RequestParam(value = "entidadId") Long entidadId,
			@RequestParam(value = "tipoDocumentoId") Long tipoDocumentoId,
			@RequestParam(value = "nroDocumento") String nroDocumento) {

		RespBase<RespListarDatosUOXServidorCivil> response = this.gestionService.listarDatosUOxSC(entidadId,
				tipoDocumentoId, nroDocumento);

		return ResponseEntity.ok(response);
	}

	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Listar Evaluados sin Evluadores en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Listar evaluados sin evaluadores", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = {
			Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/entidad/evaluadosSinEvaluadores" }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil>> buscarEvaluadosSinEvaluadoresServidorCivilEntidad(
			@PathVariable String access, @RequestParam(value = "entidadId", required = true) Long entidadId)
			throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		response = servidorCivilService.buscarEvaluadosSinEvaluadoresEntidad(parametroMap);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluadores y evaluados sin evaluar por entidad en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar Participantes Evaluadores y evaluados sin evaluar por entidad en Servidor Civil", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/entidad/evaluadores" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> buscarParticipantesServidorCivilEntidad(
			@PathVariable String access, 
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "segmentoId", required = false) Long segmentoId,
			@RequestParam(value = "uoId", required = false) Long uoId)
			throws ValidationException {
		RespBase<Object> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("segmentoId", segmentoId);
		parametroMap.put("uoId", uoId);
		response = servidorCivilService.buscarParticipantesEntidad(parametroMap);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = Constantes.SUM_OBT_LIST
			+ "Buscar Participantes Evaluados en Servidor Civil", description = Constantes.SUM_OBT_LIST
					+ "Buscar particiapantes evaluados por un evaluador", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/entidad/evaluados" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> buscarParticipantesEvaluadosServidorCivilEntidad(
			@PathVariable String access, 
			@RequestParam(value = "entidadId", required = true) Long entidadId,
			@RequestParam(value = "uoId", required = true) Long uoId,
			@RequestParam(value = "personaEvaluadorId", required = true) Long personaEvaluadorId,
			@RequestParam(value = "segmentoId", required = false) Long segmentoId)
			throws ValidationException {
		RespBase<Object> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		parametroMap.put("uoId", uoId);
		parametroMap.put(Constantes.PERSONAEVALUADORID, personaEvaluadorId);
		parametroMap.put("segmentoId", segmentoId);
		response = servidorCivilService.buscarParticipantesEvaluadosServidorCivilEntidad(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Obtener informacion del Participante por el DET_UO_ID", description = "Obtener informacion del Participante por el DET_UO_ID", tags =
	{ "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })

	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participante/info" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> buscarParticipanteByDUOId(@PathVariable String access,
			@RequestParam(value = "detUnidadOrganicaId", required = true) Long detUnidadOrganicaId,
			@RequestParam(value = "personaId", required = true) Long personaId,
			@RequestParam(value = "segmentoId", required = true) Long segmentoId,
			@RequestParam(value = "rolId", required = true) Long rolId) throws ValidationException {
		RespBase<Object> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("detUnidadOrganicaId", detUnidadOrganicaId);
		parametroMap.put("personaId", personaId);
		parametroMap.put("segmentoId", segmentoId);
		parametroMap.put("rolId", rolId);
		response = servidorCivilService.buscarParticipanteByDUOId(parametroMap);
		return ResponseEntity.ok(response);
	}
	
}
