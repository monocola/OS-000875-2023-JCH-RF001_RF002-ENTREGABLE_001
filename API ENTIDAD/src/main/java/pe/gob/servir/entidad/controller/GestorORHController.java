package pe.gob.servir.entidad.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.GestorORHService;

import javax.servlet.http.HttpServletRequest;

@RestController
@Tag(name = "GestoresORH", description = "")
public class GestorORHController {

    @Autowired
    private HttpServletRequest httpServletRequest;

    @Autowired
    private GestorORHService gestorORHService;

    @Operation(summary = "Anula un gestor", description = "Anula un gestor", tags = { "" }, security = {
            @SecurityRequirement(name = Constantes.BEARER_JWT) })
    @ApiResponses(value = {
            @ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
            @ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
                    @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
            @ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
                    @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
    @DeleteMapping(path = { Constantes.BASE_ENDPOINT + "/gestorOrh/{gestorId}" }, produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity<RespBase<Object>> anularIdioma(@PathVariable String access,
                                                         @PathVariable Long gestorId) {
        MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
        RespBase<Object> response = this.gestorORHService.anularGestorORH(jwt, gestorId);
        return ResponseEntity.ok(response);
    }


}
