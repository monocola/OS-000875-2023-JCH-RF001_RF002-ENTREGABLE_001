package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiUploadFile {
	
	private String fileBase64;
	private String fileName;
	private String extension;
	private String observacion;
	private String path;


	
}
