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
@Table(name = "TBL_BANNER", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Banner extends AuditEntity{
	@Id
	@Column(name = "BANNER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_BANNER")
	@SequenceGenerator(sequenceName = "SQ_BANNER", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_BANNER")
	private Long bannerId;
	
	@Column(name =  "DESCRIPCION")
	private String descripcion;
	
	@Column(name =  "URL_IMG")
	private String urlImg;
	
	@Column(name =  "URL_WEB")
	private String urlWeb;
	
	@Column(name =  "ORDEN")
	private Long orden;
	
}
