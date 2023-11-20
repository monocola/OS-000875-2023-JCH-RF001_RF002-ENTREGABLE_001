package pe.gob.servir.entidad.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * Clase plantilla para response
 *
 * @param <T> Clase del payload
 * @author ttorres
 */
@Getter
@Setter
@ToString
public class RespBase<T> {

	private Trace trace;
	private Status status;
	@JsonInclude(Include.NON_NULL)
	private T payload;

	public RespBase() {
		super();
		trace = new Trace();
		status = new Status();
	}

	public RespBase<T> ok(T payload) {
		RespBase<T> response = new RespBase<>();
		response.setPayload(payload);
		response.getStatus().setSuccess(Boolean.TRUE);
		return response;
	}

	/**
	 * Subclase plantilla para trazabilidad
	 *
	 * @author ttorres
	 */
	@Getter
	@Setter
	@ToString
	public static class Trace {

		private String traceId;
	}
	
	/**
	 * Subclase plantilla para status
	 *
	 * @author ttorres
	 */
	@Getter
	@Setter
	@ToString
	public static class Status {

		private Boolean success;
		private String info;
		private Error error;

		public Status() {
			super();
			error = new Error();
		}
		

		/**
		 * Subclase plantilla para error
		 *
		 * @author ttorres
		 */
		@Getter
		@Setter
		@ToString
		public static class Error implements Serializable{

			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			private String code;
			private String httpCode;
			private List<String> messages;

			public Error() {
				super();
				messages = new ArrayList<>();
			}
			
		}
	}
}
