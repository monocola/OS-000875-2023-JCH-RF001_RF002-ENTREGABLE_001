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
public class EntidadDTO {
	
	@Id
	private Long entidadId;
	
	private String descripcionEntidad;
	
	private String nombrePersona;
	
	private String apellidoPaterno;
	
	private String apellidoMaterno;
}
