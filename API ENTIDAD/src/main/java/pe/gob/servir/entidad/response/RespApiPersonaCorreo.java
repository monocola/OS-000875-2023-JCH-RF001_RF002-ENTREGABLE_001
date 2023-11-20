package pe.gob.servir.entidad.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.api.dto.ApiAuditoriaDTO;

@Getter
@Setter
@ToString
public class RespApiPersonaCorreo {		
	  
	private Long correoId;
    private Long personaId;
    private String tipoCorreo;
    private String correo;
    
	}










