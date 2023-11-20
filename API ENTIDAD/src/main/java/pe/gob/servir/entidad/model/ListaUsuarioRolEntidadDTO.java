package pe.gob.servir.entidad.model;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Data
@Entity
public class ListaUsuarioRolEntidadDTO {

	@Id
	private Long usuarioRolId;
	private Long entidadId;
	private Long usuarioId;
	private Long personaId;
	private Long rolId;
	private String usuario;
	private String correo;
	private String fechaInicioVigencia;
	private String fechaFinVigencia;
	private String estado;
	
	
}
