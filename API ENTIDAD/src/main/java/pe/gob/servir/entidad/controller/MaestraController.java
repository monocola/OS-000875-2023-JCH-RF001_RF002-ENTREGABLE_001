package pe.gob.servir.entidad.controller;

import java.util.HashMap;
import java.util.Map;

import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
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
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerParametro;
import pe.gob.servir.entidad.response.RespObtieneLista;
import pe.gob.servir.entidad.service.MaestraService;

@RestController
@Tag(name = "Maestra", description = "")
public class MaestraController {

	private static final Logger LOGGER = Logger.getLogger(MaestraController.class);

	@Autowired
	private MaestraService maestraService;


	@Operation(summary = Constantes.SUM_OBT_LIST + "Tipo Documento en API Entidad", description = Constantes.SUM_OBT_LIST
			+ "Tipo Documento en API Entidad", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/tiposparametro/parametros" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerParametro>> obtenerParametro(@PathVariable String access,
			@RequestParam(value = "tipoParametro", required = true) String tipoParametro
			/*@RequestParam(value = "codigoNumero", required = true) String codigoNumero*/) {
		LOGGER.info("Inicio Metodo: obtenerParametro...");
				
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("tipoParametro", tipoParametro);
		parametroMap.put("estadoRegistro", "1");
		parametroMap.put("codigoNumero", "4");			
		
		RespBase<RespObtenerParametro> response = maestraService.obtenerParametro(parametroMap);
		
		return ResponseEntity.ok(response);		
	}
	
	@Operation(summary = Constantes.SUM_OBT_LIST + "Tipo Documento en API Entidad", description = Constantes.SUM_OBT_LIST
			+ "Tipo Documento en API Entidad", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/maestra" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneLista>> obtenerParametros(@PathVariable String access,
			@RequestParam(value = "tipoParametro", required = true) String tipoParametro) {
		LOGGER.info("Inicio Metodo: obtenerParametro...");
		
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("tipoParametro", tipoParametro);
		RespBase<RespObtieneLista> response = maestraService.obtenerParametros(parametroMap);
		
		return ResponseEntity.ok(response);		
	}

	

}
