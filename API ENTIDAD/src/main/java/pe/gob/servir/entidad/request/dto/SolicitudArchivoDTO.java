package pe.gob.servir.entidad.request.dto;
import javax.validation.constraints.Size;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class SolicitudArchivoDTO {	
	
	private Long solicitudArchivoId;
	
	@Size(max = 100, message = "Campo nombreArchivo es inválido, máximo 100 caracter")
	private String nombreArchivo;
	
	@Size(max = 100, message = "Campo nombreRealArchivo es inválido, máximo 100 caracter")
	private String nombreRealArchivo;
	
	private Integer tipoArchivo;
	
	private String archivo;	
			
	private String estado;
}
