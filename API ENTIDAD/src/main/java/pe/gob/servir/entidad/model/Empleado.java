package pe.gob.servir.entidad.model;

import java.io.Serializable;

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
@Table(name = "TBL_EMPLEADO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Empleado  extends AuditEntity implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EMPLEADO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_EMPLEADO_ID")
	@SequenceGenerator(sequenceName = "SEQ_EMPLEADO_ID", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SEQ_EMPLEADO_ID")
	private Long empleadoId;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "PERSONA_ID")
	private Long personaId;
	
	@Column(name = "REGIMEN_LABORAL_ID")
	private Long regimenLaboral;
	
	@Column(name = "PUESTO_ID")
	private Long puestoId;
	
	@Column(name = "SINDICATO_FLAG")
	private String sindicatoFlag;
	
	@Column(name = "URL_FOTO")
	private String urlFoto;
	
}
