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
public class PaisesDTO {
	@Id
	private Integer paisId;
	private String nacionalidad;
	private String nombrePais;

}
