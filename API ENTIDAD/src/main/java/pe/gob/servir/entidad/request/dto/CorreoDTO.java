package pe.gob.servir.entidad.request.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter   
@ToString
public class CorreoDTO {
	private Long correoId;
	private Long personaId;
	private String tipoCorreo;
	private String correo;
		
		public CorreoDTO(){
			super();
		}

}
