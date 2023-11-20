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
public class CuentaEntidadRolDTO {
	
	@Id
	private Long cuentaEntidadId;
	private Long usuarioRolId;
	private String fechaInicioVigencia;
	private Integer tipoDocumento;
	private String numeroDocumento;
	
}
