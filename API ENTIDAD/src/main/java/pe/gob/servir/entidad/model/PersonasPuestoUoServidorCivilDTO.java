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
public class PersonasPuestoUoServidorCivilDTO {

	@Id
	private Long detalleuoId;
	private Long personaId;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String nombres;
	private String puestoFechaCese;

}
