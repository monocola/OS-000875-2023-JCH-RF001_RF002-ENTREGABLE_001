package pe.gob.servir.mensajeria.controller;

import java.io.IOException;
import java.time.Instant;

import javax.mail.MessagingException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.mensajeria.common.Constantes;
import pe.gob.servir.mensajeria.common.RoutingKey;
import pe.gob.servir.mensajeria.queue.producer.RabbitProducer;
import pe.gob.servir.mensajeria.request.ReqBase;
import pe.gob.servir.mensajeria.request.ReqEmail;
import pe.gob.servir.mensajeria.response.RespBase;
import pe.gob.servir.mensajeria.response.RespEnviaEmail;
import pe.gob.servir.mensajeria.service.EmailService;
import pe.gob.servir.mensajeria.service.PlantillaCorreoService;

@RestController
@Tag(name = "Correo", description = "")
public class EmailController {

	@Autowired
	EmailService emailService;
	
	@Autowired
	PlantillaCorreoService plantillaService;
	
	@Autowired
	private RabbitProducer rabbitProducer;
	
	@Operation(summary = "Envio de Correo Masivo", description = "Envio de Correo Masivo", tags = { "" },
			security = { @SecurityRequirement(name = "bearer-key") }
	)
	@ApiResponses(value = { 
			@ApiResponse(responseCode = "200", description = "operación exitosa"),
			@ApiResponse(responseCode = "500", description = "error interno", content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) 
	})
	@PostMapping(path = { Constantes.BASE_ENDPOINT+"/email" } 
			, consumes = { MediaType.APPLICATION_JSON_VALUE },produces = { MediaType.APPLICATION_JSON_VALUE }
	)
	public ResponseEntity<RespBase<RespEnviaEmail>> enviarEmailMasivo(
			@PathVariable String access,
			 @RequestBody ReqBase<ReqEmail> request) throws IOException, MessagingException {
		String jsonMessage = new ObjectMapper().writeValueAsString(request);
		rabbitProducer.writeMessage(RoutingKey.SEND_EMAIL_MASIVO, jsonMessage);
	    RespEnviaEmail respSendEmail = new RespEnviaEmail("success", "el correo estará siendo enviado en los proximos minutos", Instant.now());
	    RespBase<RespEnviaEmail> response = new RespBase<>();
	    response.setPayload(respSendEmail);
	     
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Envio de Correo de Credenciales", description = "Envio de Correo de Credenciales", tags = { "" },
			security = { @SecurityRequirement(name = "bearer-key") }
	)
	@ApiResponses(value = { 
			@ApiResponse(responseCode = "200", description = "operación exitosa"),
			@ApiResponse(responseCode = "500", description = "error interno", content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) 
	})
	@PostMapping(path = { Constantes.BASE_ENDPOINT+"/enviarCorreo" } 
			, consumes = { MediaType.APPLICATION_JSON_VALUE },produces = { MediaType.APPLICATION_JSON_VALUE }
	)
	public ResponseEntity<RespBase<RespEnviaEmail>> enviarCorreoCrendenciales(
			@PathVariable String access,
			 @RequestBody ReqBase<ReqEmail> request) throws IOException, MessagingException {
		String jsonMessage = new ObjectMapper().writeValueAsString(request);
		rabbitProducer.writeMessage(RoutingKey.SEND_EMAIL_CREDENCIALES, jsonMessage);
	    RespEnviaEmail respSendEmail = new RespEnviaEmail("success", "el correo estará siendo enviado en los proximos minutos", Instant.now());
	    RespBase<RespEnviaEmail> response = new RespBase<>();
	    response.setPayload(respSendEmail);
	     
		return ResponseEntity.ok(response);
	}
	
}
