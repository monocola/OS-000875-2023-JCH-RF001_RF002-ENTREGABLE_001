package pe.gob.servir.entidad.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.common.Constantes;

@Entity
@Table(name = "TBL_DETALLE_UO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class DetUnidadOrganica extends AuditEntity{

	@Id
	@Column(name = "DET_UO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator ="SEQ_DET_UO_ID")
	@SequenceGenerator(sequenceName = "SEQ_DET_UO_ID", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SEQ_DET_UO_ID")
	private Long detUnidadOrganicaId;
		
	@Column(name = "ORGANIGRAMA_ID")
	private Long organigramaId;
	
	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "PERSONA_ID")
	private Long personaId;
	
	@Column(name = "PUESTO_ID")
	private Long puestoId;
	
	@Column(name = "ES_RESPONSABLE")
	private String responsable;
	
	@Column(name = "EXCLUYE")
	private String excluye;

	@Column(name = "PUESTO_FECHA_INICIO")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaInicioPuesto;
	
	@Column(name = "PUESTO_FECHA_CESE")
	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private Date fechaCesePuesto;
	
	@Column(name = "TIPO_ASIGNACION")
	private Integer tipoAsignacion;
	
	@Column(name = "SEGMENTO_ID")
	private Long segmentoId;
	
	@Column(name = "ESTADO_SRV_CIV_GDR_ID")
	private Long estadoSrvCivGdrId;
	
	@Column(name = "FLAG_HABILITAR")
	private Long flagHabilitar;

	@Column(name = "ROL_ID")
	private Long rolId;
	
	@Column(name = "PERSONA_EVALUADOR_ID")
	private Long personaEvaluadorId;
	
	@Column(name =  "INDICADOR_META")
	private Long indicadorMeta;
	
	
}
