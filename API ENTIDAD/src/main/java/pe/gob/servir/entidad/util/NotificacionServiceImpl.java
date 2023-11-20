package pe.gob.servir.entidad.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.api.dto.ApiEnvioCorreoRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.service.NotificacionService;

@Service
public class NotificacionServiceImpl implements NotificacionService{
		
	@Autowired
	MaestraApiClient maestraApiClient;;
	
	@Autowired
	private VariablesSistema variablesSistema;
		
	@SuppressWarnings("unchecked")
	@Override
	public void enviarNotificacion(String asunto,String plantilla, Map<String, Object> parametros,boolean ejecutarHilo) {	
		RespBase<ApiEnvioCorreoRequestDTO> envioCorreoResquest = new RespBase<>();
		ApiEnvioCorreoRequestDTO envioCorreo = new ApiEnvioCorreoRequestDTO();
		envioCorreo.setAsunto(asunto);
		envioCorreo.setCodigoPlantilla(plantilla);
		envioCorreo.setEnviarTo(buscarCorreos(parametros,Constantes.CORREOS_ENVIO));
		envioCorreo.setCc(buscarCorreos(parametros,Constantes.CORREOS_COPIA));
		envioCorreo.setCco(buscarCorreos(parametros,Constantes.CORREOS_COPIA_OCULTA));
		Map<String, Object> valores = (Map<String, Object>) parametros.get(Constantes.PARAMETROS);
		if(valores != null) {
			valores.put(Constantes.CORREO_CONTACTO_PROPERTY, variablesSistema.correoContacto);			
		}	
		envioCorreo.setJsonParametros(JsonUtil.convertirObjetoACadenaJson(valores));
		envioCorreoResquest.setPayload(envioCorreo); 
		enviarCorreo(envioCorreoResquest);
	}
		 
	 public void enviarCorreo(RespBase<ApiEnvioCorreoRequestDTO> envioCorreoResquest){  
	  	try {
	  		RespBase<Object> responseWS = maestraApiClient.enviarCorreo(envioCorreoResquest);
	  		System.out.println("SE COMPLETO EL ENVIO DE CORREO CON HILO STATUS = "+responseWS.getStatus());
		} catch (Exception e) {						
			System.out.println(e.getMessage());
		}             
	}
		
	@SuppressWarnings("unchecked")
	public String buscarCorreos(Map<String, Object> parametros,String key) {
		List<String> correosEnvio = new ArrayList<String>();
	    if(parametros.get(key) != null){ correosEnvio = (List<String>) parametros.get(key);}	
		String enviarTo = Strings.EMPTY;
		for (int i = 0; i < correosEnvio.size(); i++) {
			if(i == 0) {
				enviarTo += correosEnvio.get(i);				
			}else {
				enviarTo += ","+correosEnvio.get(i);				
			}
		}
		if(enviarTo.equals(Strings.EMPTY)) enviarTo = null;
		return enviarTo;
	}

	@Override
	public void enviarCodigoConfirmacion(String asunto, String plantilla, String correo,
			boolean ejecutarHilo, String codigoConfirmacion) {
		RespBase<ApiEnvioCorreoRequestDTO> envioCorreoResquest = new RespBase<>();
		ApiEnvioCorreoRequestDTO envioCorreo = new ApiEnvioCorreoRequestDTO();
		envioCorreo.setAsunto(asunto);
		envioCorreo.setCodigoPlantilla(plantilla);
		envioCorreo.setEnviarTo(correo);
		Map<String, Object> valores = new HashMap<>();;
		if(valores != null) {
			valores.put(Constantes.CORREO_CONTACTO_PROPERTY, variablesSistema.correoContacto);	
			valores.put(Constantes.CORREO_CODIGO_CONFIRMACION, codigoConfirmacion);			
		}	
		envioCorreo.setJsonParametros(JsonUtil.convertirObjetoACadenaJson(valores));
		envioCorreoResquest.setPayload(envioCorreo); 
		enviarCorreo(envioCorreoResquest);
		
	}
	
	@Override
	public void enviarNotificacionCambioPass(String asunto, String plantilla, String correo,
			boolean ejecutarHilo) {
		RespBase<ApiEnvioCorreoRequestDTO> envioCorreoResquest = new RespBase<>();
		ApiEnvioCorreoRequestDTO envioCorreo = new ApiEnvioCorreoRequestDTO();
		envioCorreo.setAsunto(asunto);
		envioCorreo.setCodigoPlantilla(plantilla);
		envioCorreo.setEnviarTo(correo);
		Map<String, Object> valores = new HashMap<>();;
		if(valores != null) {
			valores.put(Constantes.CORREO_CONTACTO_PROPERTY, variablesSistema.correoContacto);		
		}
		envioCorreo.setJsonParametros(JsonUtil.convertirObjetoACadenaJson(valores));
		envioCorreoResquest.setPayload(envioCorreo); 
		enviarCorreo(envioCorreoResquest);
	}
}
