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
@Table(name = "TBL_VALIDACION_ENTIDAD", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class ValidacionEntidad extends AuditEntity{
	
	@Id
	@Column(name = "VALIDA_ENTIDAD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_VALIDACION_ENTIDAD")
	@SequenceGenerator(sequenceName = "SQ_VALIDACION_ENTIDAD", allocationSize = 1,schema = "SCH_ENTIDAD" , name = "SQ_VALIDACION_ENTIDAD")
	private Long validaEntidadId;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "TIPO_VALIDACION")
	private Integer tipoValidacion;
	
	@Column(name = "FLAG_VALIDADO")
	private String flagValidado;

}
