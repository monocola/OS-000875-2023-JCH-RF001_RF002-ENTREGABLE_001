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
@Table(name = "TBL_PROCESO_ERROR", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class ProcesoError extends AuditEntity {
	
	@Id
	@Column(name = "PROCESO_ERROR_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PROCESO_ERROR_ID")
	@SequenceGenerator(sequenceName = "SEQ_PROCESO_ERROR_ID", allocationSize = 1, schema = "SCH_ENTIDAD", name = "SEQ_PROCESO_ERROR_ID")
	private Long procesoErrorId;

	@Column(name = "PROCESO_ID")
	private Long procesoId;

	@Column(name = "DETALLE_ERROR")
	private String detalleError;

}