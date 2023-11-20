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
public class ComboPuesto {
	@Id
	private Long id;
	private String descripcion;
	private Long organigramaId;

}
