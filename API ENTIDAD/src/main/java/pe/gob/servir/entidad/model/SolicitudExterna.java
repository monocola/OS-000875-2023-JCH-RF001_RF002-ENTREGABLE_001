package pe.gob.servir.entidad.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;


import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
@Table(name = "TBL_SOLICITUD_ENTIDADES_EXTERNAS", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class SolicitudExterna extends AuditEntity{
	
	@Id
	@Column(name = "SOLICITUD_ENT_EXT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SOLICITUD_ENTIDAD_EXT")
	@SequenceGenerator(sequenceName = "SQ_SOLICITUD_ENTIDAD_EXT", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_SOLICITUD_ENTIDAD_EXT")
	private Long solicitudEntidadExtId;

	@Column(name = "RUC_ENTIDAD")
	private String rucEntidad;
	@Column(name = "RAZON_SOCIAL")
	private String razonSocial;
	@Column(name = "ABREVIATURA_ENTIDAD")
	private String abreviatura;
	@Column(name = "NOMBRE_ENTIDAD")
	private String nombreEntidad;
	@Column(name = "NIVEL_GOBIERNO_ID")
	private Long nivelGobiernoId;
	@Column(name = "NIVEL_GOBIERNO")
	private String nivelGobierno;
	@Column(name = "SECTOR_ID")
	private Long sectorId;
	@Column(name = "SECTOR")
	private String sector;
	
	@Column(name = "TIPO_ENTIDAD_ID")
	private Long tipoEntidadId;
	@Column(name = "TIPO_ENTIDAD")
	private String tipoEntidad;
	@Column(name = "URL_LOGO_ENTIDAD")
	private String urlLogoEntidad;
	
	@Column(name = "TIPO_DOCUMENTO")
	private Integer tipoDocumento;
	@Column(name = "NUMERO_DOCUMENTO")
	private String numeroDocumento;
	
	@Column(name = "APELLIDO_PATERNO")
	private String apellidoPaterno;
	@Column(name = "APELLIDO_MATERNO")
	private String apellidoMaterno;
	@Column(name = "NOMBRES")
	private String nombres;
	@Column(name = "FECHA_NACIMIENTO")
	private String fechaNacimiento;
	@Column(name = "TELEFONO_FIJO")
	private String telefonoFijo;
	@Column(name = "ANEXO")
	private String anexo;
	@Column(name = "CELULAR")
	private String celular;
	
	@Column(name = "CORREO_ELECTRONICO")
	private String correoElectronico;
	@Column(name = "UUID_DOCUMENTO")
	private String uuidDocumento;
	@Column(name = "ESTADO_SOLICITUD")
	private Integer estadoSolicitud;

	@Column(name = "SINDICATO")
	private String sindicato;
	@Column(name = "CORREO_GESTOR_GDR")
	private String correoGestorGdr;

	@Column(name = "SOLICITUD_OBS")
	private String solicitudObs;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;

	@Column(name = "RUTA_ARCHIVO")
	private String rutaArchivo;
}
