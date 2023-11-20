package pe.gob.servir.entidad.feign.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import pe.gob.servir.entidad.api.dto.ApiEnvioCorreoRequestDTO;
import pe.gob.servir.entidad.api.dto.ApiFileServerDTO;
import pe.gob.servir.entidad.api.dto.ApiUploadFile;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.response.RespApiFile;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerPais;
import pe.gob.servir.entidad.response.RespObtieneLista;
import pe.gob.servir.entidad.response.RespUploadFile;

@FeignClient(name = "maestraApi", url = "${jboss.private.base.url.maestra}")
public interface MaestraApiClient {

	// @formatter:off
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_TIPO_PARAMETRO, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespObtieneLista> obtieneParametros(@PathVariable("tipoParametro") String tipoParametro);

	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_ENVIAR_CORREO, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	RespBase<Object> enviarCorreo(RespBase<ApiEnvioCorreoRequestDTO> request);

	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_SUBIR_ARCHIVO, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespUploadFile> uploadFile(RespBase<ApiUploadFile> request);

	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_INS_IMAGEN, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiFile> insertImagen(ReqBase<ApiFileServerDTO> request);

	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_PAIS_QUERY, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespObtenerPais> buscarPais(@RequestParam(value = "paisId") Long paisId,
			@RequestParam(value = "nombrePais") String nombrePais);
	// @formatter:on

	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_BAJAR_ARCHIVO, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	RespBase<String> downloadFileBase64(ReqBase<String> request);

}