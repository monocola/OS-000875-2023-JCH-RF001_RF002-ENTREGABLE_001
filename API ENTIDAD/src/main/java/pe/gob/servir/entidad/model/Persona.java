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
@Table(name = "TBL_PERSONA", schema = "SCH_PERSONA")
@Getter
@Setter
@ToString
public class Persona extends AuditEntity{

	@Id
	@Column(name = "PERSONA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator ="SEQ_PERSONA_ID")
	@SequenceGenerator(sequenceName = "SEQ_PERSONA_ID", allocationSize = 1, schema = "SCH_PERSONA" ,name = "SEQ_PERSONA_ID")
	private Long personaId;
		
	@Column(name = "TIPO_PERSONA")
	private String tipopPersona;
	
	@Column(name = "DOCUMENTO_ID")
	private Long documentoId;
	
	@Column(name = "DIRECCION_ID")
	private Long direccionId;
	
	@Column(name = "TELEFONO_ID")
	private Long telefonoId;
	
	@Column(name = "CORREO_ID")
	private Long correoId;
	
	@Column(name = "WEB_ID")
	private Long webId;
	
	@Column(name = "ESTADO_PERSONA")
	private String estadoPersona;
		
}
