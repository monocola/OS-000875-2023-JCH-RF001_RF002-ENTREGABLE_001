package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiFileServerDTO {
	
	String fileBase64;
    String fileName;
    String extension;
    String observacion;
    Double ratioDeCambio;
    String path;
    Boolean resize;
    
}
