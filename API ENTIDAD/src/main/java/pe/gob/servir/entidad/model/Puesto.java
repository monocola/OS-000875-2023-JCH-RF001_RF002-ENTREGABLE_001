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
@Table(name = "TBL_PUESTO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Puesto extends AuditEntity{

	@Id
	@Column(name = "PUESTO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator ="SEQ_PUESTO_ID")
	@SequenceGenerator(sequenceName = "SEQ_PUESTO_ID", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SEQ_PUESTO_ID")
	private Long puestoId;
		
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "DESCRIPCION")
	private String descripcion;
	
	@Column(name = "ORGANIGRAMA_ID")
	private Long organigramaId;
	
	@Column(name = "ES_JEFE")
	private String esJefe;		
}
