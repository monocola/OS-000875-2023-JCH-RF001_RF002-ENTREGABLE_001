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
public class ExisteOrganigramaDTO {
	
	@Id
	private Long organigramaId;
	private String descripcion;
	private String sigla;
	
}
