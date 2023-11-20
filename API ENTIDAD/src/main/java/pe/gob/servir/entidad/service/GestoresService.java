package pe.gob.servir.entidad.service;


import pe.gob.servir.entidad.model.GestorDTO;
import pe.gob.servir.entidad.request.ReqActualizaGestorORH;
import pe.gob.servir.entidad.request.ReqGestorORH;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespGestores;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface GestoresService {
	
	RespBase<RespGestores> registrarGestores(ReqGestorORH request, MyJsonWebToken token);
	RespBase<Object> listaGestoresOrh(Long entidadId);
	RespBase<RespGestores> actualizaGestores(ReqActualizaGestorORH request, MyJsonWebToken token);
	RespBase<GestorDTO> getGestorId(Long gestorId);
}
