package pe.gob.servir.mensajeria.service;

import org.hibernate.service.spi.ServiceException;

import pe.gob.servir.mensajeria.model.PlantillaCorreo;

public interface PlantillaCorreoService {
	PlantillaCorreo obtenerPlantilla(String codigoPlantilla) throws ServiceException;
}
