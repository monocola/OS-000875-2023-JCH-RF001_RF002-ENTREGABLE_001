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
@Table(name = "TBL_SOLICITUD_REVISION_DET", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class SolicitudRevisionDetalle extends AuditEntity{
	
	@Id
	@Column(name = "SOLICITUD_REVISION_DET_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SOLICITUD_REVISION_DET")
	@SequenceGenerator(sequenceName = "SQ_SOLICITUD_REVISION_DET", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_SOLICITUD_REVISION_DET")
	private Long solicitudRevisionDetalleId;

	@Column(name = "SOLICITUD_REVISION_ID")
	private Long solicitudRevisionId;
	
	@Column(name = "OBSERVA_ID")
	private Long observacionId;

}
