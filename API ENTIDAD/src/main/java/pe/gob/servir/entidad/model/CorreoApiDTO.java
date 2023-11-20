package pe.gob.servir.entidad.model;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class CorreoApiDTO {
		
	@Id
	private Long personaId;
	private Long correoId;
	private String tipoCorreo;
	private String correo;
}
