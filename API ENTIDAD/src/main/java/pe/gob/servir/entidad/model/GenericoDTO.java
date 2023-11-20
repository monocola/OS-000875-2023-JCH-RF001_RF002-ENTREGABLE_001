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
public class GenericoDTO {
	@Id	
	public Integer graficoid;
	public String codigoTexto;
	public String descripcion;

}
