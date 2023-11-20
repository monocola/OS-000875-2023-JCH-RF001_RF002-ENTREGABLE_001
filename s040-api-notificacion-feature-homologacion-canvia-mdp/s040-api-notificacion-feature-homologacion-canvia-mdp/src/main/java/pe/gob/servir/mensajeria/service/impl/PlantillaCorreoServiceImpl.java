package pe.gob.servir.mensajeria.service.impl;

import static org.jboss.resteasy.resteasy_jaxrs.i18n.LogMessages.LOGGER;

import org.hibernate.service.spi.ServiceException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.mensajeria.model.PlantillaCorreo;
import pe.gob.servir.mensajeria.repository.PlantillaCorreoRepository;
import pe.gob.servir.mensajeria.service.PlantillaCorreoService;

@Service
public class PlantillaCorreoServiceImpl implements PlantillaCorreoService{

	@Autowired
	PlantillaCorreoRepository plantillaRepository;
	
	
	@Override
	public PlantillaCorreo obtenerPlantilla(String codigoPlantilla) throws ServiceException {
		  try {
	        
	            PlantillaCorreo plantillaCorreo = plantillaRepository.obtenerPlantilla(codigoPlantilla);
	            return plantillaCorreo;
	        } catch (Exception e) {
	            LOGGER.error(e.getMessage(), e);
	            return null;
	        }
	}

}
