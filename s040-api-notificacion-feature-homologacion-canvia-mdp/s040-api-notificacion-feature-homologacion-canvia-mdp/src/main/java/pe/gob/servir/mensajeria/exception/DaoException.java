package pe.gob.servir.mensajeria.exception;

/**
 * Clase Exception de la capa Dao
 * 
 * @author ttorres
 *
 */
@SuppressWarnings("serial")
public class DaoException extends Exception {

	private final String code;
	private final int httpCode;

	public DaoException(String mensaje, String code, int httpCode) {
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
