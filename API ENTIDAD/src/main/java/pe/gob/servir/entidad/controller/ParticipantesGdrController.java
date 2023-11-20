package pe.gob.servir.entidad.controller;

import java.util.HashMap;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespBuscarParticipante;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadoEvaluador;
import pe.gob.servir.entidad.service.ServidorCivilService;


@RestController
@Tag(name = "ParticipantesGdr", description = "")
public class ParticipantesGdrController {

	@Autowired
	ServidorCivilService servidorCivilService;
	
	@Operation(summary = "Obteber evaluador y evaluado UO", description = "Obteber evaluador y evaluado UO", tags = {
							"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participantes/evaluadoEvaluador" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipanteEvaluadoEvaluador>> buscarEvaluadoEvaluador(
			@PathVariable String access, 
			@RequestParam(value = "detUOId", required = true) Long detUOId
			) throws ValidationException {
		RespBase<RespBuscarParticipanteEvaluadoEvaluador> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("detUOId", detUOId);
		response = servidorCivilService.buscarEvaluadoEvaluador(parametroMap);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Obtener participante UO", description = "Obtener participante UO", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/servidorCivil/participante" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespBuscarParticipante>> buscarParticipante(
			@PathVariable String access,
			@RequestParam(value = "detUOId", required = true) Long detUOId,
			@RequestParam(value = "segmentoId", required = true) Long segmentoId) 
					throws ValidationException {
		RespBase<RespBuscarParticipante> response;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("detUOId", detUOId);
		parametroMap.put("segmentoId", segmentoId);
		response = servidorCivilService.buscarParticipante(parametroMap);
		return ResponseEntity.ok(response);
	}

}
