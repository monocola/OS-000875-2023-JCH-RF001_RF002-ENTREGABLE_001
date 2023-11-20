package pe.gob.servir.entidad.api.dto;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiPersonaEdicionRequestDTO<T> {
	
	T persona;
	
	List<Correo> correos;
	
	public ApiPersonaEdicionRequestDTO() {

		this.correos= new ArrayList<Correo>();
				
	}

	@Getter
	@Setter
	@ToString
	public static class PersonaNaturalEdicion {

		private Long personaId;
		private String sexo;
		private LocalDate fechaNacimiento;
		
		public PersonaNaturalEdicion() {
			//constructor vacio
		}
		
	}
	
	
	@Getter
	@Setter
	@ToString
	public static class Correo {
		private Long correoId;
		private String tipoCorreo;
		private String correo;

		public Correo() {
			super();
		}

		public Correo(Long correoId, String tipoCorreo, String correo) {
			super();
			this.correoId = correoId;
			this.tipoCorreo = tipoCorreo;
			this.correo = correo;
		}

		public Correo(String tipoCorreo, String correo) {
			super();
			this.tipoCorreo = tipoCorreo;
			this.correo = correo;
		}		

	}
	
}
