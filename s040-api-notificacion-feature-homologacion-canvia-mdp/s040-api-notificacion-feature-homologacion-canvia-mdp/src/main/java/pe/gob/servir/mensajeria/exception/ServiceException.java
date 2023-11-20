package pe.gob.servir.mensajeria.exception;



/**
 * Clase Exception de la capa Service
 * 
 * @author ttorres
 *
 */

@SuppressWarnings("serial")
public class ServiceException extends Exception {

	private final String code;
	private final int httpCode;

	public ServiceException(String mensaje) {
		super(mensaje);
		this.code = null;
		this.httpCode = 500;
	}

	public ServiceException(String mensaje, String code, int httpCode) {
		super(mensaje);
		this.code = code;
		this.httpCode = httpCode;
	}

	public String getCode() {
		return code;
	}

	public int getHttpCode() {
		return httpCode;
	}
}
