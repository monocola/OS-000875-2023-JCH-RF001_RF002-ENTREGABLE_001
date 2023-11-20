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
@Table(name = "TBL_DIRECCION", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Direccion extends AuditEntity {
	
	@Id
	@Column(name = "DIRECCION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_DIRECCION")
	@SequenceGenerator(sequenceName = "SQ_DIRECCION", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_DIRECCION")
	private Long direccionId;

	@Column(name = "UBIGEO_ID")
	private Integer ubigeoId;

	@Column(name = "REFERENCIA_DIRECCION")
	private String referenciaDireccion;

	@Column(name = "ZONA_GEOGRAFICA_ID")
	private Integer zonaGeografica;
	
	@Column(name = "DIRECCION")
	private String direccion;
}
