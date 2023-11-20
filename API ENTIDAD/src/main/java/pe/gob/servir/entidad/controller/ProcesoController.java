package pe.gob.servir.entidad.controller;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
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
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqRegistrarProcesoAsincrono;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespRegistrarProcesoAsincrono;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.ProcesoAsincronoService;

@RestController
@Tag(name = "Proceso asincrono", description = "")
public class ProcesoController {
	private static final Logger LOGGER = Logger.getLogger(ProcesoController.class);

	@Autowired
	private HttpServletRequest httpServletRequest;

	@Autowired
	private ProcesoAsincronoService procesoAsincronoService;

	@Operation(summary = "Registra un proceso asincrono", description = "Registra un proceso asincrono", tags = {
			"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/registrar/procesoAsincrono" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespRegistrarProcesoAsincrono>> registrarProcesoAsincrono(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqRegistrarProcesoAsincrono> request) throws Exception {
		
		LOGGER.info("Ejecutando servicio POST registrarProcesoAsincrono");

		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespRegistrarProcesoAsincrono> response = this.procesoAsincronoService.registrarProcesoAsincrono(jwt, request);
		return ResponseEntity.ok(response);
	}

}