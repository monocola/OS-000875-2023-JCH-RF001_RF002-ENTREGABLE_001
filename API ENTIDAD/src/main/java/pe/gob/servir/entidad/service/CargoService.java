package pe.gob.servir.entidad.service;

import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtieneCargo;

public interface CargoService {
	
	RespBase<RespObtieneCargo> obtieneCargo();
	
}
