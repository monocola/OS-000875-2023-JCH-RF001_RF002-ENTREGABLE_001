package pe.gob.servir.entidad.service;

import java.util.Map;

import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerParametro;
import pe.gob.servir.entidad.response.RespObtieneLista;

public interface MaestraService {

	RespBase<RespObtenerParametro> obtenerParametro(Map<String, Object> parametroMap);
	
	RespBase<RespObtieneLista> obtenerParametros(Map<String, Object> parametroMap);
	
}
