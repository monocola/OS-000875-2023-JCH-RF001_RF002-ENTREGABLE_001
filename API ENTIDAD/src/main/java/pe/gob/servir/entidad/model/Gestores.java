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
@Table(name = "TBL_GESTORES", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Gestores extends AuditEntity{
	@Id
	@Column(name = "GESTOR_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_GESTORES")
	@SequenceGenerator(sequenceName = "SEQ_GESTORES", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SEQ_GESTORES")
	private Long gestorId;

	@Column(name = "PERSONA_ID")
	private Long personaId;
	
	@Column(name = "CORREO_ID")
	private Long correoId;	
	
	@Column(name = "USUARIO_ID") 
	private Long usuarioId;
	
	@Column(name = "ROL_ID")
	private Long rolId;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;

	@Column(name = "TELEFONO_ID")
	private Long telefonoId;	
	
	@Column(name = "CELULAR_ID")
	private Long celularId;	
	
	@Column(name = "CELULAR")
	private String celular;
	
	@Column(name = "NUMERO_ANEXO")
	private String anexoTelefono;
	
	@Column(name = "TIPO_TELEFONO")
	private String tipoCelular;
}
