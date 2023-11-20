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
@Table(name = "TBL_CUENTA_ENTIDAD", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class CuentaEntidad extends AuditEntity{
	
	@Id
	@Column(name = "CUENTA_ENTIDAD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_CUENTA_ENTIDAD")
	@SequenceGenerator(sequenceName = "SQ_CUENTA_ENTIDAD", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_CUENTA_ENTIDAD")
	private Long cuentaEntidadId;

	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "PERSONA_ID")
	private Long personaId;
	
	@Column(name = "AREA_ID")
	private Long areaId;
	
	@Column(name = "PUESTO_TRABAJO_ID")
	private String puestoTrabajoId;
	
	@Column(name = "CORREO_ID")
	private Long correoId;
	
	@Column(name = "TELEFONO_ID")
	private Long telefonoId;
	
	@Column(name = "USUARIO_ID")
	private Long usuarioId;
	
	@Column(name = "MOTIVO")
	private String motivo;
}
