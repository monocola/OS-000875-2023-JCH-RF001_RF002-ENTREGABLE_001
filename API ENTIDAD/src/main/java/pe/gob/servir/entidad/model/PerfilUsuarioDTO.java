package pe.gob.servir.entidad.model;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class PerfilUsuarioDTO {

	@Id
	private Long personaId;
	private Long entidadId;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String nombres;
	private String tipoDocumento;
	private String numeroDocumento;
	private String telefono;
	private String genero;
	private Date fechaNacimiento;
	private String correoInstitucional;
	private String correoAlternativo;
	private Long regimenId;
	private String regimenLaboral;
	private String sindicato;
	private String urlFoto;

}