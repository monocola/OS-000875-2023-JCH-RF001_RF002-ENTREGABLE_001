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
@Table(name = "TBL_CORREO", schema = "SCH_PERSONA")
@Getter
@Setter
@ToString
public class Correo extends AuditEntity{

	@Id
	@Column(name = "CORREO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator ="SEQ_CORREO_ID")
	@SequenceGenerator(sequenceName = "SEQ_CORREO_ID", allocationSize = 1, schema = "SCH_PERSONA" ,name = "SEQ_CORREO_ID")
	private Long correoId;
		
	@Column(name = "PERSONA_ID")
	private Long personaId;
	
	@Column(name = "TIPO_CORREO")
	private String tipoCorreo;
	
	@Column(name = "CORREO")
	private String correo;
	
}
