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
public class GestorDTO {

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
	private Long correoId;
	private String numeroTelefono;
	private Long telefonoId;
	private String tipoTelefono;
	private String anexoTelefono;
	private String fechaNacimiento;
	private Long usuarioId;
	private Long rolId;
	private String estadoRegistro;
	private String estado;
	private String numeroCelular;
	private Long celularId;
	private String tipoCelular;
	
}
