package pe.gob.servir.mensajeria.response;

import java.io.Serializable;
import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.InstantSerializer;

/**
 * Created by Developer02 on 21/04/2020.
 */
@SuppressWarnings("serial")
public class RespEnviaEmail implements Serializable {

	private String estado;
	private String mensaje;

	@JsonSerialize(using = InstantSerializer.class)
	@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ssZ", timezone = "UTC")
	private Instant fechaEnvio;

	public RespEnviaEmail(String estado, String mensaje, Instant fechaEnvio) {
		this.estado = estado;
		this.mensaje = mensaje;
		this.fechaEnvio = fechaEnvio;
	}

	public String getEstado() {
		return estado;
	}

	public void setEstado(String estado) {
		this.estado = estado;
	}

	public String getMensaje() {
		return mensaje;
	}

	public void setMensaje(String mensaje) {
		this.mensaje = mensaje;
	}

	public Instant getFechaEnvio() {
		return fechaEnvio;
	}

	public void setFechaEnvio(Instant fechaEnvio) {
		this.fechaEnvio = fechaEnvio;
	}
}
