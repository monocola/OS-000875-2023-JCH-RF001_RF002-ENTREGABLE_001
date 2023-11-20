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
public class UsuarioRolEntidadPersonaDTO {

	@Id
	private Long personaId;
	private Long usuarioId;
	private Long rolId;
	private Long usuarioRolId;
	private String estado;
	private Long entidadId;
	
}
