package pe.gob.servir.entidad.request.dto;

import java.time.LocalDate;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter   
@ToString
public class CorreoEdicionDTO {
	private Long correoId;
	private Long personaId;
	private String tipoCorreo;
	private String correo;
	private String usuarioModificaicion;
	private LocalDate fechaModificacion;
		
		public CorreoEdicionDTO(){
			super();
		}

}
