package pe.gob.servir.entidad.response;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespMensaje {

	private String mensaje;
	private Long bannerId;
}
