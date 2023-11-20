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
@Table(name = "TBL_ORGANIGRAMA", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Organigrama extends AuditEntity{
	
	@Id
	@Column(name = "ORGANIGRAMA_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_ORGANIGRAMA")
	@SequenceGenerator(sequenceName = "SQ_ORGANIGRAMA", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_ORGANIGRAMA")
	private Long organigramaId;

	@Column(name = "ENTIDAD_ID")
	private Long entidadId;
	
	@Column(name = "AREA_ID")
	private Long areaId;
	
	@Column(name = "SEDE_ID")
	private Long sedeId;
	
	@Column(name = "PADRE_ORGANIGRAMA_ID")
	private Long padreOrganigramaId;
	
	@Column(name = "PUESTO")
	private String puesto;
	
	@Column(name = "ORDEN")
	private Integer orden;
	
	@Column(name = "NIVEL")
	private Long nivel;
	
	@Column(name = "TIPO_ORGANO_UO_ID")
	private Long tipoOrganoUoId;
	
	@Column(name = "NATURALEZA_ORGANO")
	private Long naturalezaOrgano;

	@Column(name = "NIVEL_GOBIERNO_ID")
	private Long nivelGobiernoId;
	
	@Column(name = "DESCRIPCION")
	private String descripcion;
	
	@Column(name = "DESCRIPCION_CORTA")
	private String descripcionCorta;
	
	@Column(name = "SIGLA")
	private String sigla;
	
	@Column(name = "PERSONA_RESPONSABLE_ID")
	private Long personaResponsableId;
	
	@Column(name = "TELEFONO_ID")
	private Long telefonoId;
	
	@Column(name = "CORREO_ID")
	private Long correoId;
	

}
