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
@Table(name = "TBL_DETALLE_ENCARGATURA", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class DetEncargaturaDTO extends AuditEntity{

	@Id
	@Column(name = "DETALLE_ENCARGATURA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator ="SEQ_DET_ENCARGATURA_ID")
	@SequenceGenerator(sequenceName = "SEQ_DET_ENCARGATURA_ID", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SEQ_DET_ENCARGATURA_ID")
	private Long detEncargaturaId;
		
	@Column(name = "DET_UO_ID")
	private Long detUoId;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "MOTIVO_ID")
	private Long motivoId;

	
	
}
