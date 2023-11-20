package pe.gob.servir.entidad.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.common.Constantes;

@Entity
@Table(name = "TBL_SOLICITUD_PERSONA", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class SolicitudPersona{
	
	@Id 
	@Column(name = "SOLICITUD_PERSONA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SOLICITUD_PERSONA")
	@SequenceGenerator(sequenceName = "SQ_SOLICITUD_PERSONA", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_SOLICITUD_PERSONA")
	private Long solicitudPersonaId;	
	@Column(name = "SOLICITUD_ENTIDAD_ID")
	private Long solicitudEntidadId;	
	@Column(name = "ROL_ENTIDAD_ID")
	private Long rolEntidadId;
	@Column(name = "TIPO_PERSONA")
	private Integer tipoPersona;
	@Column(name = "TIPO_DOCUMENTO")
	private Integer tipoDocumento;
	@Column(name = "NUMERO_DOCUMENTO")
	private String numeroDocumento;
	@Column(name = "VALIDAR")
	private String validar;
	@Column(name = "RAZON_SOCIAL")
	private String razonSocial;
	@Column(name = "NOMBRE_COMERCIAL")
	private String nombreComercial;
	@Column(name = "SIGLA")
	private String sigla;
	@Column(name = "NOMBRES")
	private String nombres;
	@Column(name = "APELLIDO_PATERNO")
	private String apellidoPaterno;
	@Column(name = "APELLIDO_MATERNO")
	private String apellidoMaterno;
	@Column(name = "APELLIDO_CASADA")
	private String apellidoCasada;
	@Column(name = "FECHA_NACIMIENTO")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaNacimiento;
	@Column(name = "SEXO")
	private String sexo;
	@Column(name = "ESTADO_CIVIL")
	private Integer estadoCivil;
	@Lob
	@Column(name = "IMAGEN")
	private String imagen;
	@Column(name = "CARGO_ID")
	private Long cargoId;
	@Column(name = "PUESTO_TRABAJO_ID")
	private Long puestoTrabajoId;
	@Column(name = "FECHA_INSCRIPCION")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaInscripcion;
	@Column(name = "FECHA_INICIO_ACTIVIDADES")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaInicioActividades;
	@Column(name = "ACTIVIDAD_ECONOMICA_PRINCIPAL")
	private String actividadEconomicaPrincipal;
	@Column(name = "CONDICION_CONTRIBUYENTE")
	private String condicionContribuyente;
	@Column(name = "ESTADO_CONTRIBUYENTE")
	private String estadoConstribuyente;
	@Column(name = "PAIS_ID")
	private Long paisId;
	@Column(name = "UBIGEO_ID")
	private Long ubigeoId;	
	@Column(name = "DIRECCION_COMPLETA")
	private String direccionCompleta;
	@Column(name = "REFERENCIA_DIRECCION")
	private String referenciaDireccion;
	@Column(name = "CORREO_PRINCIPAL")
	private String correoPrincipal;
	@Column(name = "CORREO_SECUNDARIO")
	private String correoSecundario;
	@Column(name = "CORREO_LABORAL")
	private String correoLaboral;
	@Column(name = "TELEFONO_FIJO")
	private String telefonoFijo;
	@Column(name = "CELULAR_PRINCIPAL")
	private String celularPrincipal;
	@Column(name = "CELULAR_SECUNDARIO")
	private String celularSecundario;
	@Column(name = "CELULAR_LABORAL")
	private String celularLaboral;
	@Column(name = "RUTA_PAGINA_WEB")
	private String rutaPaginaWeb;
	@Column(name = "LUGAR_NACIMIENTO")
	private String lugarNacimiento;	
	
	@Transient
	List<SolicitudArchivo> listaArchivo = new ArrayList<>();
	
	@Transient
	private Long cuentaClienteId;
	
	@Transient
	private Long personaId;
	
	@Transient
	private String descripcionCargo;
	
	@Transient
	private String descripcionPais;
	
	@Transient
	private String descripcionTipoDoc;
			
	@Transient
	private Long correoId;
	
	@Transient
	private Long telefonoId;
	
	@Transient
	private Long usuarioId;
	
	public SolicitudPersona(Long solicitudEntidadId) {
		this.solicitudEntidadId = solicitudEntidadId;
	}
	
	public SolicitudPersona(Long solicitudEntidadId,Integer tipoPersona) {
		this.solicitudEntidadId = solicitudEntidadId;
		this.tipoPersona = tipoPersona;
	}
	
	public SolicitudPersona() {
		
	}
}
