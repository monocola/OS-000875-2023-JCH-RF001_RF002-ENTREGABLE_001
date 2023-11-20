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
@Table(name = "TBL_MEDIO_CONTACTO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class MedioContacto extends AuditEntity {
	
	@Id
	@Column(name = "MEDIO_CONTACTO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_MEDIOCONTACTO")
	@SequenceGenerator(sequenceName = "SQ_MEDIOCONTACTO", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_MEDIOCONTACTO")
	private Long medioContactoId;

	@Column(name = "ENTIDAD_ID")
	private Integer entidadId;
	
	@Column(name = "TIPO_MEDIO_CONTACTO")
	private String tipoMedioContacto;
	
	@Column(name = "FLAG_PRINCIPAL")
	private String flagPrincipal;
	
	@Column(name = "VALOR_MEDIO_CONTACTO")
	private String valorMedioContacto;
	
	@Column(name = "ANEXO_MEDIO_CONTACTO")
	private String anexoMedioContacto;
	
}
