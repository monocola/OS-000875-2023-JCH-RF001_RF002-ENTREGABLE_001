package pe.gob.servir.entidad.service;

import java.util.Map;

import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqSede;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerSede;
import pe.gob.servir.entidad.response.RespSede;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface SedeService {
	
	RespBase<RespObtenerSede> buscarSedeByFilter(Map<String, Object> parametroMap);
	
	RespBase<RespSede> guardarSede(ReqBase<ReqSede> request, MyJsonWebToken token,
			Long sedeId);
	
	RespBase<Object> eliminarSede(MyJsonWebToken token, Long sedeId,String estado);
}
