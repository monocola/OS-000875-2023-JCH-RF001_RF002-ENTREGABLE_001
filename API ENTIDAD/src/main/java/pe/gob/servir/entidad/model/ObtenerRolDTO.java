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
public class ObtenerRolDTO {
	@Id	
	private Long nroRoles;
	private Long usuarioRolId;
	private Long rolId;		
	private String nombreRol;
	private Long cuentaId;
	private String estadoId;
	private String descripcionEstado;
	private String fechaAltaRol;
	private String fechaBajaRol;

}
