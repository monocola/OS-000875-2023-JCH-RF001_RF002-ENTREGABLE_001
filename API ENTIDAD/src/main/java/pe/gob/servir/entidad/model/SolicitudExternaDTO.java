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
public class SolicitudExternaDTO {

	@Id
	private Long solicitudEntidadExtId;
	
	private String rucEntidad;
	private String razonSocial;
	private String nombreEntidad;
	private Integer tipoDocumento;
	private String numeroDocumento;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String nombres;
	
	
	private String uuidDocumento;
	private String abreviatura;
	private Long nivelGobiernoId;
	private String nivelGobierno;
	private Long sectorId;
	private String sector;
	private Long tipoEntidadId;
	private String tipoEntidad;
	private String urlLogoEntidad;
	private String fechaNacimiento;
	private String telefonoFijo;
	private String anexo;
	private String celular;
	private String correoElectronico;
	
	private Integer estadoId;
	private String estado;
	private String fechaSolicitud;
	private String nombreCompleto;
	
	private String solicitudObs;
	private String correoGestorGdr;
	private Long entidadId;
	private Integer sindicato;
	
}
