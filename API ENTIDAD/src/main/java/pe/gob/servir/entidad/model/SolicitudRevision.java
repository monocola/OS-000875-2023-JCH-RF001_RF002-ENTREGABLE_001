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
@Table(name = "TBL_SOLICITUD_REVISION", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class SolicitudRevision extends AuditEntity{
	
	@Id
	@Column(name = "SOLICITUD_REVISION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SOLICITUD_REVISION")
	@SequenceGenerator(sequenceName = "SQ_SOLICITUD_REVISION", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_SOLICITUD_REVISION")
	private Long solicitudRevisionId;

	@Column(name = "SOLICITUD_ENTIDAD_ID")
	private Long solicitudEntidadId;
	
	@Column(name = "DESCRIPCION")
	private String descripcion;
	
	@Column(name = "ENVIO_CORREO")
	private String envioCorreo;
	
	@Column(name = "ESTADO_REVISION")
	private Integer estadoRevision;
	
	@Column(name = "CODIGO")
	private String codigo;
	
}
