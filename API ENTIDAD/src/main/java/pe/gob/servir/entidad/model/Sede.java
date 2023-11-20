package pe.gob.servir.entidad.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
@Table(name = "TBL_SEDE", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Sede extends AuditEntity {
	
	@Id
	@Column(name = "SEDE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_SEDE")
	@SequenceGenerator(sequenceName = "SQ_SEDE", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_SEDE")
	private Long sedeId;
	
	@Column(name = "DIRECCION_ID")
	private Long direccionId;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "SEDE_PADRE_ID")
	private Long sedePadreId;
	
	@Column(name =  "NOMBRE_SEDE")
	private String nombreSede;
	
	@Column(name = "AMBITO")
	private String ambito;
	
	@Column(name = "ANEXO")
	private String anexo;
	
	@Column(name = "TELEFONO")
	private String telefono;
	
	@Column(name = "NOMBRE_REPRESENTANTE")
	private String nombreRepresentante;
	
	@Transient
	Direccion direccion = new Direccion();

}
