package pe.gob.servir.entidad.api.dto;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ReqEmailAttachment {
	
	@NotNull
	@NotEmpty
	private String fileName;		// Nombre del archivo adjunto
	private String content;			// Contenido del archivo adjunto en base64
	private String urlFile;			// URL de ubicacion del archivo
	private byte[] contentFile;

}