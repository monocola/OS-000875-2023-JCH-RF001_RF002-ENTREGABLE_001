package pe.gob.servir.entidad.controller;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
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
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.SolicitudPersona;
import pe.gob.servir.entidad.request.ReqActualizaSolicitud;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqCreaSolicitud;
import pe.gob.servir.entidad.request.ReqInsObservacion;
import pe.gob.servir.entidad.request.dto.UbicacionPersonaDTO;
import pe.gob.servir.entidad.response.RespActualizaSolicitud;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespCreaSolicitud;
import pe.gob.servir.entidad.response.RespObtenerCorreo;
import pe.gob.servir.entidad.response.RespObtieneSolicitud;
import pe.gob.servir.entidad.response.RespObtieneSolicitudEntidad;
import pe.gob.servir.entidad.response.RespObtieneSolicitudEntidadById;
import pe.gob.servir.entidad.response.RespSolicitudDuplicado;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.SolicitudService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@RestController
@Tag(name = "SolicitudEntidad", description = "")
public class SolicitudController {
	
	@Autowired
	private SolicitudService solicitudService;
	
	@Autowired
	private HttpServletRequest httpServletRequest;
	
	@Operation(summary = "Crea una Solicitud", description = "Crea una Solicitud", tags = { "" },
				security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad" }, 
				 consumes = {MediaType.APPLICATION_JSON_VALUE },
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
	public ResponseEntity<RespBase<RespCreaSolicitud>> creaSolicitud(
			@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqCreaSolicitud> request) throws IOException {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<RespCreaSolicitud> response = solicitudService.creaSolicitudEntidad(request,jwt);
		return ResponseEntity.ok(response);

	}
	
	@Operation(summary = Constantes.SUM_OBT_LIST+"solicitud entidad", description = Constantes.SUM_OBT_LIST+"solicitud entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad" },
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneSolicitudEntidad>> ObtieneSolicitudEntidad(
			@PathVariable String access) {
		RespBase<RespObtieneSolicitudEntidad> response = solicitudService.obtieneSolicitudEntidad();
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Validar Solicitud Duplicado", description = "Validar si una solicitud ya se encuentra registrada", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/validarsolicitud/{ruc}/{doc}" },
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
	public ResponseEntity<RespBase<RespSolicitudDuplicado>> validarSolicitudDuplicado(
			@PathVariable String access,
			@PathVariable Long ruc,
			@PathVariable Long doc) {
		RespBase<RespSolicitudDuplicado> response = solicitudService.validarSolicitudDuplicado(ruc, doc);
		return ResponseEntity.ok(response);
	
	}
	
	@Operation(summary = "Obtiene una solicitud entidad", description = "Obtiene una solicitud entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/{solicitudId}" }, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneSolicitudEntidadById>> ObtieneSolicitudEntidad(
			@PathVariable String access, 
			@PathVariable Long solicitudId) {
		RespBase<RespObtieneSolicitudEntidadById> response = solicitudService.obtieneSolicitudEntidadById(solicitudId);
		return ResponseEntity.ok(response);

	}	

	@Operation(summary = Constantes.SUM_OBT_LIST+"solicitud entidad By filtros", description = Constantes.SUM_OBT_LIST+"solicitud entidad by filtros", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/filter"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneSolicitud>> ObtieneSolicitudFilter(
			@PathVariable String access,
			@RequestParam(value = "razonSocial", required = false)String razonSocial,@RequestParam(value = "ruc", required = false) String ruc,
			@RequestParam(value = "sector", required = false)Integer sector,@RequestParam(value = "nivelGobierno", required = false) Integer nivelGobierno,
			@RequestParam(value = "fechaInicio", required = false) Date fechaInicio,@RequestParam(value = "fechaFin", required = false) Date fechaFin,
			@RequestParam(value = "estado", required = false)Integer estado,@RequestParam(value = "departamento", required = false) Integer departamento,
			@RequestParam(value = "provincia", required = false)Integer provincia,@RequestParam(value = "distrito", required = false) Integer distrito) {
		RespBase<RespObtieneSolicitud> response = new RespBase<>();
		if(validarParametros(razonSocial, ruc, sector, nivelGobierno, fechaInicio, fechaFin, estado, departamento, provincia, distrito)) {
			Map<String, Object> parametroMap = new HashMap<>();	
			parametroMap.put("razonSocial", razonSocial);	
			parametroMap.put("ruc", ruc);
			parametroMap.put("sector", sector);
			parametroMap.put("nivelGobierno", nivelGobierno);
			parametroMap.put("fechaInicio", fechaInicio);
			parametroMap.put("fechaFin", fechaFin);
			parametroMap.put("estado", estado);
			parametroMap.put("departamento", departamento);
			parametroMap.put("provincia", provincia);
			parametroMap.put("distrito", distrito);
			response = solicitudService.buscarSolicituByFilter(parametroMap);
		}else {
			response = ParametrosUtil.setearResponse(response,Boolean.FALSE,Constantes.RESPONSE_MESSAGE);	
		}
		return ResponseEntity.ok(response);		
	}
	
	@Operation(summary = "Actualizacion de los datos de Solicitud", description = "Actualiza Datos de la Solicitud", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/{solicitudId}"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespActualizaSolicitud>> ActualizarSoliPersona(
			@PathVariable String access,
			@PathVariable Long solicitudId,
			@Valid @RequestBody ReqBase<ReqActualizaSolicitud> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespActualizaSolicitud> response = solicitudService.actualizarSolicitud(request, jwt,solicitudId);
		return ResponseEntity.ok(response);		

	}
			
	
	 
	private boolean validarParametros(String razonSocial,String ruc,Integer sector,Integer nivelGobierno
			,Date fechaInicio, Date fechaFin,Integer estado,Integer departamento,Integer provincia,Integer distrito) {
		if(StringUtils.isEmpty(razonSocial) && StringUtils.isEmpty(ruc) && sector==null && nivelGobierno==null && fechaInicio == null
				&& fechaFin == null && estado==null && departamento==null && provincia==null && distrito==null){
			return false;
		}
		if ((fechaInicio != null && fechaFin == null) || (fechaInicio == null && fechaFin != null)){
			return false;
		}

		if (fechaInicio != null || fechaFin != null){
			return ParametrosUtil.validarFechaInicioFin(fechaInicio, fechaFin);
		}
		return true;

	}

	@Operation(summary = "dar alta a una Solicitud", description = "dar alta a una Solicitud", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/darAlta/{solicitudId}" }, 
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
	public ResponseEntity<RespBase<Object>> darAlta(
			@PathVariable String access,
			@PathVariable Long solicitudId) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<Object> response = solicitudService.darAltaSolicitud(jwt,solicitudId);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Observar una Solicitud", description = "Observar una Solicitud", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/observacion/{solicitudId}" }, 
				 consumes = {MediaType.APPLICATION_JSON_VALUE },
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
	public ResponseEntity<RespBase<Object>> registrarObservacion(
			@PathVariable String access,
			@PathVariable Long solicitudId,
			@Valid @RequestBody ReqBase<ReqInsObservacion> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<Object> response = solicitudService.registrarObservacion(request,jwt,solicitudId);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "validar si la solicitud del proveedor puede ser modificada", description = "Obtiene una solicitud entidad", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/{codigo}/{id}" }, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneSolicitudEntidadById>> validarCodigoActualizarSolicitud(
			@PathVariable String access, 
			@PathVariable String codigo,
			@PathVariable Long id) {
		RespBase<RespObtieneSolicitudEntidadById> response = new RespBase<RespObtieneSolicitudEntidadById>();
		if (StringUtils.isEmpty(codigo)) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.DEBE_INGRESAR + " un codigo actualizacion");
		}else if (id == null) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.DEBE_INGRESAR + " una solicitudId");
		}else {
			response = solicitudService.validarcodActualizacionSolicitud(codigo, id);
		}
		return ResponseEntity.ok(response);
	}	
	
	@Operation(summary = "Actualizacion de los datos de ubicacion de la persona", description = "Actualiza Datos de ubicacion de la persona", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/persona/{solicitudPersonaId}"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<SolicitudPersona>> actualizarSolicitudPersona(
			@PathVariable String access,
			@PathVariable Long solicitudPersonaId,
			@Valid @RequestBody ReqBase<UbicacionPersonaDTO> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<SolicitudPersona> response = solicitudService.actualizarUbicacionPersona(request, jwt,solicitudPersonaId);
		return ResponseEntity.ok(response);		

	}
	
	@Operation(summary = "Validar Correo Electronico", description = "Validar Correo Electronico de la Solicitud", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/solicitudentidad/validarcorreo/{correo}"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
		public ResponseEntity<RespBase<RespObtenerCorreo>> validarSolitudCorreo(
				@PathVariable String access, 
				@PathVariable String correo) {
		RespBase<RespObtenerCorreo> response = solicitudService.validarCorreoSolicitud(correo);
		return ResponseEntity.ok(response);

	}
	
	@Operation(summary = "Notificar Cambio de Contraseña", description = "Notificar cuando el usuario haga un cambio de contraseña", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })	
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/entidad/not-cambio-pass/{correo}"}, 
				produces = { MediaType.APPLICATION_JSON_VALUE })
		public ResponseEntity<RespBase<RespObtenerCorreo>> notificarCambioPass(
				@PathVariable String access, 
				@PathVariable String correo) {
		RespBase<RespObtenerCorreo> response = solicitudService.notificarCorreoCambioPass(correo);
		return ResponseEntity.ok(response);

	}
}
