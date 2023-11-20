package pe.gob.servir.entidad.service;

import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqProcesoAsincrono;
import pe.gob.servir.entidad.request.ReqRegistrarProcesoAsincrono;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespRegistrarProcesoAsincrono;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface ProcesoAsincronoService {

	RespBase<RespRegistrarProcesoAsincrono> registrarProcesoAsincrono (MyJsonWebToken jwt, ReqBase<ReqRegistrarProcesoAsincrono> request) throws Exception;
	
	void procesarCargaMasivaServidoresCiviles(ReqProcesoAsincrono request);
}