package pe.gob.servir.entidad.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.common.Constantes;

@Entity
@Table(name = "TBL_SOLICITUD", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Solicitud extends AuditEntity{
	
	@Id
	@Column(name = "SOLICITUD_ENTIDAD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SOLICITUD")
	@SequenceGenerator(sequenceName = "SQ_SOLICITUD", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_SOLICITUD")
	private Long solicitudEntidadId;

	@Column(name = "SECTOR")
	private Long sector;
	@Column(name = "NIVEL_GOBIERNO")
	private Long nivelGobierno;
	@Column(name = "TIPO")
	private Integer tipo;
	@Column(name = "TIPO_ENTIDAD")
	private Integer tipoEntidad;
	@Column(name = "ACEPTO_TERMINOS_CONDICIONES")
	private String aceptoTerminosCondiciones;
	@Column(name = "NUMERO_INTENTOS")
	private Integer numeroIntentos;
	@Column(name = "APLICACION_ID")
	private Long aplicacionId;
	@Column(name = "ESTADO_SOLICITUD")
	private Integer estadoSolicitud;
	
	@Column(name = "FECHA_ALTA")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaAlta;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Transient
	private String descripcionSector;
	
	@Transient
	private String descripcionNivelGobierno;
	
	@Column(name = "FECHA_BAJA")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaBaja;
}
