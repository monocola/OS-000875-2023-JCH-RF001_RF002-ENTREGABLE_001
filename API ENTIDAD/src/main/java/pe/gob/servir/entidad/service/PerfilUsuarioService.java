package pe.gob.servir.entidad.service;

import java.util.Map;

import javax.validation.Valid;

import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEditaPerfilUsuario;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtienePerfilUsuario;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface PerfilUsuarioService {

	RespBase<RespObtienePerfilUsuario> obtenerPerfilUsuario(Map<String, Object> parametroMap) throws ValidationException;

	RespBase<Object> editarPerfilUsuario(@Valid ReqBase<ReqEditaPerfilUsuario> request, MyJsonWebToken jwt) throws ValidationException;
	
}