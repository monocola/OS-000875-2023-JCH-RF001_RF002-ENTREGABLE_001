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
public class ResumenServidoresCivilesGDRDTO {
	@Id	
	private Integer entidadId;
	private Integer totalgestoresgdr;
	private Integer totalservidoresgdr;
	private Integer totalevaluadores;
	private Integer totalevaluados;

}
