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
public class CuentaEntidadDTO {
	
	@Id
	private Long cuentaEntidadId;
	private Long personaId;
	private Integer tipoDocumento;
	private String numeroDocumento;
	
}
