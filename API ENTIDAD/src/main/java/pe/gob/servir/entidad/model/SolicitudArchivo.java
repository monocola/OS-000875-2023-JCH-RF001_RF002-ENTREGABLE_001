
package pe.gob.servir.entidad.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
@Table(name = "TBL_SOLICITUD_ARCHIVO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class SolicitudArchivo extends AuditEntity{
	
	@Id
	@Column(name = "SOLICITUD_ARCHIVO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SOLICITUD_ARCHIVO")
	@SequenceGenerator(sequenceName = "SQ_SOLICITUD_ARCHIVO", allocationSize = 1,schema = "SCH_ENTIDAD" , name = "SQ_SOLICITUD_ARCHIVO")
	private Long solicitudArchivoId;
	
	@Column(name = "SOLICITUD_ENTIDAD_ID")
	private Long solicitudEntidadId;
	
	@Column(name = "NOMBRE_ARCHIVO")
	private String nombreArchivo;
	
	@Column(name = "NOMBRE_REAL_ARCHIVO")
	private String nombreRealArchivo;
		
	@Column(name = "TIPO_ARCHIVO")
	private Integer tipoArchivo;
	
	@Lob
	@Column(name = "ARCHIVO")
	private String archivo;
	
	@Column(name = "SOLICITUD_PERSONA_ID")
	private Long solicitudPersonaId;
	
	@Transient
	private String rutaUpload;
}

