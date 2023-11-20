package pe.gob.servir.entidad.service;

import java.util.Map;

public interface NotificacionService {
	
	void enviarNotificacion(String asunto, String plantilla, Map<String, Object> parametros,
			boolean ejecutarHilo);
	
	void enviarCodigoConfirmacion(String asunto, String plantilla, String parametros,
			boolean ejecutarHilo,String codigoConfirmacion);
	
	void enviarNotificacionCambioPass(String asunto, String plantilla, String parametros,
			boolean ejecutarHilo);
	
}
