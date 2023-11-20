package pe.gob.servir.entidad.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jboss.logging.Logger;
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
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.multipart.MultipartFile;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.ComboDTO;
import pe.gob.servir.entidad.model.Encrypt;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.request.ReqActualizarSolicitudExterna;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqObservaSolicitudExt;
import pe.gob.servir.entidad.request.ReqRegistrarSolicitudExterna;
import pe.gob.servir.entidad.request.ReqValidaSolicitudExt;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespListarSolicitudExterna;
import pe.gob.servir.entidad.response.RespRegistrarSolicitudExterna;
import pe.gob.servir.entidad.response.RespRespSubirArchivoNgnx;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.SolicitudExternaService;

@RestController
@Tag(name = "SolicitudExterna", description = "")
public class SolicitudExternaController {
	private static final Logger LOGGER = Logger.getLogger(SolicitudExternaController.class);

	@Autowired
	private HttpServletRequest httpServletRequest;

	@Autowired
	private SolicitudExternaService solicitudExternaService;

	@Operation(summary = "Busqueda de solicitudes de las Entidades externas", description = "Busqueda de solicitudes de las Entidades externas", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/externa/{tipoDocumento}/{numeroDocumento}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> ObtieneSolicitudesExternasEntidad(
			@PathVariable String access,
			@PathVariable Integer tipoDocumento,
			@PathVariable String numeroDocumento) {
		RespBase<Object> response = solicitudExternaService.buscarSolicitudEntidadExt(tipoDocumento, numeroDocumento);
		return ResponseEntity.ok(response);

	}

	@Operation(summary = "Registra una solicitud externa", description = "Registra una solicitud externa", tags = {
			"" }, security = {
					@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/registrar/solicitudExterna" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespRegistrarSolicitudExterna>> registrarSolicutdExterna(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqRegistrarSolicitudExterna> request) {

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespRegistrarSolicitudExterna> response = this.solicitudExternaService.registrarSolicitudExterna(jwt,
				request);
		return ResponseEntity.ok(response);
	}
	   
	@Operation(summary = "Registra una solicitud externa", description = "Registra una solicitud externa", tags = {
	"" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
		@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
		@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
				@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
		@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
				@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/actualizar/solicitudExterna" }, consumes = {
		MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespRegistrarSolicitudExterna>> updateSolicutdExterna(@PathVariable String access,
		@Valid @RequestBody ReqBase<ReqRegistrarSolicitudExterna> request) {
	
	MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
	RespBase<RespRegistrarSolicitudExterna> response = this.solicitudExternaService.actualizarSolicitudExterna(jwt,
			request);
	return ResponseEntity.ok(response);
	}

	@Operation(summary = "Listar solicitudes externas", description = "Listar solicitudes externas", tags = {
			"" }, security = {
					@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/externa/solicitudExt/filtrar" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespListarSolicitudExterna>> listarSolicitudExterna(@PathVariable String access,
			@RequestParam(value = "rucEntidad", required = false) String rucEntidad,
			@RequestParam(value = "razonSocial", required = false) String razonSocial,
			@RequestParam(value = "nombreCompleto", required = false) String nombreCompleto,
			@RequestParam(value = "estadoSolicitud", required = false) Integer estadoSolicitud,
			@RequestParam(value = "tipoDocumento", required = false) Integer tipoDocumento,
			@RequestParam(value = "numeroDocumento", required = false) String numeroDocumento,
			@RequestParam(value = "anio", required = false) String anio,
			@RequestParam(value = "solicitudExtId", required = false) Long solicitudExtId) {
		LOGGER.info("Metodo filtrarSolicitudesExternas");
		RespBase<RespListarSolicitudExterna> response;
		Map<String, Object> parametroMap = new HashMap<String, Object>();
		parametroMap.put("rucEntidad", rucEntidad);
		parametroMap.put("razonSocial", razonSocial);
		parametroMap.put("nombreCompleto", nombreCompleto);
		parametroMap.put("estadoSolicitud", estadoSolicitud);
		parametroMap.put("tipoDocumento", tipoDocumento);
		parametroMap.put("numeroDocumento", numeroDocumento);
		parametroMap.put("anio", anio);
		parametroMap.put("solicitudExtId", solicitudExtId);
		response = this.solicitudExternaService.filtrarSolicitudExterna(parametroMap);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Busqueda de solicitudes de las Entidades externas", description = "Busqueda de solicitudes de las Entidades externas", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/externa/solicitudExt/{solicitudExternaId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<SolicitudExternaDTO>> getSolicitudExternaId(
			@PathVariable String access,
			@PathVariable Long solicitudExternaId) {
		RespBase<SolicitudExternaDTO> response = solicitudExternaService.getSolicitudExternaId(solicitudExternaId);
		return ResponseEntity.ok(response);
 
	}

	@Operation(summary = "Lista de años - solicitudes de entidades externas", description = "Busqueda de solicitudes de las Entidades externas", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/externa/anio/solicitudExt" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<List<ComboDTO>>> getAnioSolExt(
			@PathVariable String access) {
		RespBase<List<ComboDTO>> response = solicitudExternaService.getAnioSolExt();
		return ResponseEntity.ok(response);

	}

	@Operation(summary = "Valida y aprueba una solicitud externa", description = "Valida y aprueba una solicitud externa", tags = {
			"" }, security = {
					@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/externa/solicitudExt/validar" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> validarSolicitudExterna(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqValidaSolicitudExt> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		Long solicitudExtId = request.getPayload().getSolicitudExtId();
		RespBase<Object> response = this.solicitudExternaService.aceptarSolicitudExterna(jwt, solicitudExtId, request);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Rechazar una solicitud externa", description = "Rechazar una solicitud externa", tags = {
			"" }, security = {
					@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/externa/solicitudExt/rechazar" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> rechazarSolicitudExterna(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqValidaSolicitudExt> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		Long solicitudExtId = request.getPayload().getSolicitudExtId();
		RespBase<Object> response = this.solicitudExternaService.rechazarSolicitudExterna(jwt, solicitudExtId);
		return ResponseEntity.ok(response);
	}

	@Operation(summary = "Observar una solicitud externa", description = "Observar una solicitud externa", tags = {
			"" }, security = {
					@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/externa/solicitudExt/observar" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> observarSolicitudExterna(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqObservaSolicitudExt> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		Long solicitudExtId = request.getPayload().getSolicitudExtId();
		RespBase<Object> response = this.solicitudExternaService.observarSolicitudExterna(jwt, solicitudExtId, request);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Encryptar ID", description = "Encryptar ID", tags = {
	        "" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
    @ApiResponses(value = {
	        @ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
	        @ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
			        @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
	        @ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
			        @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
    @GetMapping(path = { Constantes.BASE_ENDPOINT + "/externa/encryptSolicitudExt/{solicitudExternaId}" }, produces = {
	       MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<RespBase<Encrypt>> getEncryptSolicitudExternaId(
	        @PathVariable String access,
	        @PathVariable Long solicitudExternaId) {
        RespBase<Encrypt> response = solicitudExternaService.getEncryptSolicitudExternaId(solicitudExternaId);
        return ResponseEntity.ok(response);

    }

	@Operation(summary = "Sube un archivo al servidor Alfresco", description = "Sube un archivo al servidor Alfresco", tags = {
			"" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/uploadFile/ngnx" }, consumes = {
			MediaType.MULTIPART_FORM_DATA_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespRespSubirArchivoNgnx>> subirArchivoAlfresco(@PathVariable String access,
																					   @RequestParam(value = "archivo") MultipartFile archivo,
																					   @Valid @RequestParam Map<String, String> request) throws Exception {

		RespBase<RespRespSubirArchivoNgnx> response = this.solicitudExternaService.guardarArchivosNgnx(archivo, request);
		return ResponseEntity.ok(response);
	}

	}
