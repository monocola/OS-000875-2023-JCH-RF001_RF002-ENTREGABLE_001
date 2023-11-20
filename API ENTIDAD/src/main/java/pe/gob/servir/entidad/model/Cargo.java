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
@Table(name = "TBL_CARGO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Cargo extends AuditEntity{
	
	@Id
	@Column(name = "CARGO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_CARGO")
	@SequenceGenerator(sequenceName = "SQ_CARGO", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_CARGO")
	private Long cargoId;

	@Column(name = "DESCRIPCION")
	private String descripcion;
	
	@Column(name = "CODIGO")
	private String codigo;
		
}
