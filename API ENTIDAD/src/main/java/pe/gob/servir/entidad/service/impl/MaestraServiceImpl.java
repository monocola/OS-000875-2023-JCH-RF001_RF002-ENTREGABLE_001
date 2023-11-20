package pe.gob.servir.entidad.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.model.ListaMaestrasDTO;
import pe.gob.servir.entidad.repository.MaestraRepository;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerParametro;
import pe.gob.servir.entidad.response.RespObtieneLista;
import pe.gob.servir.entidad.response.RespObtieneLista.Parametro;
import pe.gob.servir.entidad.service.MaestraService;

@Service
public class MaestraServiceImpl implements MaestraService {

	private static final Logger LOGGER = Logger.getLogger(MaestraServiceImpl.class);

	
	@Autowired
	private MaestraApiClient maestraApiClient; 
	
	@Autowired
	private MaestraRepository maestraRepository;

	
	@Override
	public RespBase<RespObtenerParametro> obtenerParametro(Map<String, Object> parametroMap) {
		LOGGER.info("Metodo obtenerParametro...");
		List<ListaMaestrasDTO> lista = maestraRepository.obtenerParametro(parametroMap);
		RespObtenerParametro respPayload = new RespObtenerParametro();
		respPayload.setListaMaestra(lista);
		return new RespBase<RespObtenerParametro>().ok(respPayload);
	}
	
	@Override
	public RespBase<RespObtieneLista> obtenerParametros(Map<String, Object> parametroMap) {
		LOGGER.info("Metodo obtenerParametro...");
		String tipoParametro = (String)parametroMap.get("tipoParametro");
		RespBase<RespObtieneLista> respPayload =  new RespBase<RespObtieneLista>();
		List<Parametro> results = new ArrayList<Parametro>();
		respPayload = maestraApiClient.obtieneParametros(tipoParametro);
		RespObtieneLista lista = respPayload.getPayload();
		for (Parametro item : lista.getItems()) {
			if (item.getCodigoNumero().equals(1) || item.getCodigoNumero().equals(4)) {
				results.add(item);
			}
		}
		lista.setItems(results);
		respPayload.setPayload(lista);
		
		return respPayload;		
	}

}
