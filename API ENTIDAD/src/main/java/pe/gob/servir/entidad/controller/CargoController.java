package pe.gob.servir.entidad.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
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
import pe.gob.servir.entidad.response.RespObtieneCargo;
import pe.gob.servir.entidad.service.CargoService;

@RestController
@Tag(name = "Cargo", description = "")
public class CargoController {
		
	@Autowired
	private CargoService cargoService;
			
	@Operation(summary = Constantes.SUM_OBT_LIST+"cargos", description = Constantes.SUM_OBT_LIST+"cargos", tags = { "" },
			   security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT+"/cargo" }, 
						 produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtieneCargo>> ObtieneCargo(
			@PathVariable String access) {
		RespBase<RespObtieneCargo> response = cargoService.obtieneCargo();
		return ResponseEntity.ok(response);
	}
}
