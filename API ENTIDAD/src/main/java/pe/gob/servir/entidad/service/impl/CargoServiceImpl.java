package pe.gob.servir.entidad.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.model.Cargo;
import pe.gob.servir.entidad.repository.CargoRepository;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtieneCargo;
import pe.gob.servir.entidad.service.CargoService;

@Service
public class CargoServiceImpl implements CargoService {
	
	@Autowired
	private CargoRepository cargoRepository;
	
	@Override
	public RespBase<RespObtieneCargo> obtieneCargo() {
		List<Cargo> listaCargo = cargoRepository.findAll();		
		RespObtieneCargo respPayload = new RespObtieneCargo();
		respPayload.setListaCargo(listaCargo);
		return new RespBase<RespObtieneCargo>().ok(respPayload);
	}
}
