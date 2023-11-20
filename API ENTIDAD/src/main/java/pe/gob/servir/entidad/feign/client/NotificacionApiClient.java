package pe.gob.servir.entidad.feign.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import pe.gob.servir.entidad.api.dto.ReqEmail;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.response.RespBase;

@FeignClient(name = "notificacionApi", url = "${jboss.private.base.url.notificacion}")
public interface NotificacionApiClient {
	
	@PostMapping(path = { Constantes.ENDPOINT_ENVIO_CORRO},
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	RespBase<Object> enviarCorreo(ReqBase<ReqEmail> request);
	
	
	@PostMapping(path = { Constantes.ENDPOINT_ENVIO_CORREO_CRENDENCIALES},
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	RespBase<Object> enviarCorreoCredenciales(ReqBase<ReqEmail> request);

	@PostMapping(path = { Constantes.ENDPOINT_ENVIO_CORREO_NOTI_EXT},
	produces = {MediaType.APPLICATION_JSON_VALUE},
	consumes = {MediaType.APPLICATION_JSON_VALUE})
	RespBase<Object> enviarCorreoSolicitudExt(ReqBase<ReqEmail> request);
}
