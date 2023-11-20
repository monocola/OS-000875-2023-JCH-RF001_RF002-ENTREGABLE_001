package pe.gob.servir.entidad.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
@Table(name = "TBL_ENTIDAD", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Entidad extends AuditEntity{
	
	@Id
	@Column(name = "ENTIDAD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_ENTIDAD")
	@SequenceGenerator(sequenceName = "SQ_ENTIDAD", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_ENTIDAD")
	private Long entidadId;

	@Column(name = "PERSONA_ID")
	private Long personaId;
	
	@Column(name = "ADMIN_TALENTO_ID") 
	private Long adminTalentoId;
	
	@Column(name = "FECHA_ALTA")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaAlta;
	
	@Column(name = "UBIGEO_ID")
	private Long ubigeoId;
	
	@Column(name = "NIVEL_GOBIERNO_ID")
	private Long nivelGobiernoId;
	
	@Column(name = "SECTOR_ID")
	private Long sectorId;
	
	@Column(name = "TIPO_ENTIDAD_ID")
	private Integer tipoEntidadId;
	
	@Column(name = "COMPARTE_RUC")
	private String comparteRuc;
	
	@Column(name = "DESCRIPCION_ENTIDAD")
	private String descripcionEntidad;
	
	@Column(name = "URL_LOGO_ENTIDAD")
	private String urlLogoEntidad;
	
	@Column(name = "URL_WEB_ENTIDAD")
	private String urlWebEntidad; 

	@Column(name = "DIRECCION_ID")
	private Long direccionId; 
	
	@Column(name = "FLAG_ACTUALIZA")
	private String flagActualiza; 
	
	@Column(name = "SIGLAS")
	private String sigla;
		
	@Transient
	private String numeroDocumentoEntidad;
	
	@Transient
	List<MedioContacto> medioContactoList = new ArrayList<>();
	
	@Transient
	Direccion direccion = new Direccion();
	
	@Column(name = "URL_PORTADA_ENTIDAD")
	private String urlPortadaEntidad;	
		
	@Column(name = "TIPO_ENTIDAD_PUB_ID")
	private Integer tipoEntidadPubId;	
	
	@Column(name = "NRO_SINDICATOS")
	private Integer nroSindicatos;
}