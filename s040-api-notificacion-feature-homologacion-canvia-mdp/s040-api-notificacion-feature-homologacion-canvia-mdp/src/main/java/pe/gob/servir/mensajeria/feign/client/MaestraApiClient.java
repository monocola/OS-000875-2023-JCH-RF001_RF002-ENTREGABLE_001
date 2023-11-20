package pe.gob.servir.mensajeria.feign.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import pe.gob.servir.mensajeria.response.RespBase;
import pe.gob.servir.mensajeria.response.RespObtieneLista;

@FeignClient(name = "maestraApi", url = "${maestra.private.base.url}")
public interface MaestraApiClient {

	// @formatter:off
	@RequestMapping(method = RequestMethod.GET, value = "/v1/tiposparametro/{tipoParametro}/parametros", 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespObtieneLista<Object>> obtieneParametros(
			@PathVariable("tipoParametro") String tipoParametro);
	// @formatter:on
}
