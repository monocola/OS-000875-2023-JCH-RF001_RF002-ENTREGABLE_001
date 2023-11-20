package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class LogoDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Integer flag;
	private String fileBase64;
	private String fileName;	
	// @Override
	// public String toString() {
	// 	return "LogoDTO [flag=" + flag + ", fileBase64=" + fileBase64 + ", fileName=" + fileName + "]";
	// }
}
