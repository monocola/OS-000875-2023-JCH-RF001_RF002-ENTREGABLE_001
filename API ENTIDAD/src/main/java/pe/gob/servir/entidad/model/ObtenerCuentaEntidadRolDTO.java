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
public class ObtenerCuentaEntidadRolDTO {
	

	@Id
	private Integer cuentaEntidadId;
	private Integer entidadId;
	private Integer rolId;	
	private Integer usuarioId;	
	private Integer personaId;
	private String nombreCompleto;
	private String rolNombre;

}
