package pe.gob.servir.entidad.model;

import java.time.LocalDate;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class ListaGestorDTO {

	@Id
	private Long gestorId;
	private Long entidadId;
	private Long personaId;
	private Long tipoDocumento;
	private String numeroDocumento;
	private String nombres;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String nombreCompleto;
	private String correo;
	private String numeroTelefono;
	private LocalDate fechaNacimiento;
	private Long usuarioId;
	private Long rolId;
	private String estadoRegistro;
	private String estado;
	// private Long telefonoId;
	// private String numeroCelular;
	// private Long celularId;
}
