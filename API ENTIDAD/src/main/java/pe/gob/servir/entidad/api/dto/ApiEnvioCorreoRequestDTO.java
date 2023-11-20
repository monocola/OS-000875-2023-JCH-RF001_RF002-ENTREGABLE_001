package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiEnvioCorreoRequestDTO {
	
	String codigoPlantilla;
	String enviarTo;
	String asunto;
	String cc;
	String cco;
	String jsonParametros;
	
}
