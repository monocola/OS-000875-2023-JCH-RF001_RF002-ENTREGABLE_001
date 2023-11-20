package pe.gob.servir.entidad.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespListaBanners;
import pe.gob.servir.entidad.response.RespMensaje;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.BannerService;



@RestController
@Tag(name = "Banner", description = "")
public class BannerController {
	
	@Autowired
	BannerService bannerService;
	
	@Autowired
	private HttpServletRequest httpServletRequest;
	
	
	
	@Operation(summary = "Crear Banner", description = "Crear Banner", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/banner" })
	public ResponseEntity<RespBase<RespMensaje>> registrarBanner(@PathVariable String access,
			@RequestParam(value = "file",required = false) MultipartFile file,
			@RequestParam(value = "urlWeb", required = false) String urlWeb) {
		
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		 
			RespBase<RespMensaje> response =bannerService.guardarBanner(file,urlWeb, jwt);
			return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Obtener lista de banners", description = "Obtener lista de banners", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/banners" })
	public ResponseEntity<RespBase<RespListaBanners>> listarBanners(@PathVariable String access
		) {
		RespBase<RespListaBanners> response =  bannerService.obtenerBanners();
			return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Inactivar Banner", description = "Inactivar Banner", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@DeleteMapping(path = { Constantes.BASE_ENDPOINT + "/banner/{bannerId}" })
	public ResponseEntity<RespBase<RespMensaje>> inactivarBanner(@PathVariable String access,
	 
			@PathVariable(value = "bannerId", required = true) Long bannerId) {
		
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		 
		RespBase<RespMensaje>  response =bannerService.inactivarBanner(bannerId, jwt);
			return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Actualiza Banner URL", description = "Actualiza Banner URL", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/banner/{bannerId}" })
	public ResponseEntity<RespBase<RespMensaje>> actualizaBannerURL(@PathVariable String access,
			@PathVariable(value = "bannerId", required = true) Long bannerId,
			@RequestParam(value = "urlWeb", required = false) String urlWeb) {
		
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		 
		RespBase<RespMensaje>  response =bannerService.actualizarBannerURL(bannerId, urlWeb,jwt);
			return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Actualiza Banner orden", description = "Actualiza Banner orden", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT + "/banner/{bannerId}/{orden}" })
	public ResponseEntity<RespBase<RespMensaje>> actualizaBannerOrden(@PathVariable String access,
			@PathVariable(value = "bannerId", required = true) Long bannerId,
			@PathVariable(value = "orden", required = true) Long orden) {
		  
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		 
		RespBase<RespMensaje> response =bannerService.actualizarBannerOrden(bannerId, orden,jwt);
			return ResponseEntity.ok(response);
	}
}
