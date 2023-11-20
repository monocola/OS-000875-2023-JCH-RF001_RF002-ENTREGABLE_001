package pe.gob.servir.entidad.service;

import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface GestorORHService {

    RespBase<Object> anularGestorORH (MyJsonWebToken jwt, Long gestorId);

}
