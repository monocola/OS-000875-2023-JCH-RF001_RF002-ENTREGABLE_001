package pe.gob.servir.entidad.model;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
@Table
@Getter
@Setter
@ToString
public class CorreoPersonaDTO {
	
	@Id
	private Long personaId;
	private String apePaterno;
	private String apeMaterno;
	private String nombres;
	private String correo;
	private Long rango;
}
