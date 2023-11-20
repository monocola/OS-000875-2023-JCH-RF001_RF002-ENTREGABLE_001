package pe.gob.servir.entidad.response;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespApiFile {

	private String tipoAcceso;
	private String fileName;
	private String extension;
	private String pathRelative;
	private String observacion;
}
