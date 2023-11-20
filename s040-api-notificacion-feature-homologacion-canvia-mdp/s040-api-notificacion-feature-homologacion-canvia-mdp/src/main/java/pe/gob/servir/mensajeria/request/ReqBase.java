package pe.gob.servir.mensajeria.request;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

//import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Getter;
import lombok.Setter;

/**
 * Clase plantila para request
 * 
 * @author ttorres
 *
 * @param <T>
 *            Clase del payload
 */
//@JsonIgnoreProperties(ignoreUnknown = true)
@Getter
@Setter
public class ReqBase<T> {

	private Trace trace;
	@NotNull(message = "Campo payload es obligatorio")
	@Valid
	private T payload;

	public ReqBase() {
		super();
//		trace = new Trace();
	}

	/**
	 * Subclase plantilla para trazabilidad
	 * 
	 * @author ttorres
	 *
	 */
	@Getter
	@Setter
	public static class Trace {

		@NotNull(message = "Campo traceId es obligatorio")
		private String traceId;
	}
}
