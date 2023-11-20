package pe.gob.servir.entidad.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Entity
@Table(name = "TBL_ARCHIVO", schema = "SCH_ENTIDAD")
@Getter
@Setter
@ToString
public class Archivo extends AuditEntity{
	
	@Id
	@Column(name = "ARCHIVO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SQ_ARCHIVO")
	@SequenceGenerator(sequenceName = "SQ_ARCHIVO", allocationSize = 1, schema = "SCH_ENTIDAD" ,name = "SQ_ARCHIVO")
	private Long archivoId;
	
	@Column(name = "REPRESENTANTE_ENTIDAD_ID")
	private Long representanteEntidadId;
	
	@Column(name = "NOMBRE_ARCHIVO")
	private String nombreArchivo;
	
	@Column(name = "NOMBRE_REAL_ARCHIVO")
	private String nombreRealAArchivo;

	@Column(name = "TIPO_ARCHIVO")
	private Integer tipoArchivo;
	
	@Column(name = "EXTENSION_ARCHIVO")
	private String extensionArchivo;
	
	@Column(name = "ORDEN")
	private Integer orden;
	
	@Column(name = "PESO_ARCHIVO_KB")
	private Double pesoArchivoKB;
	
	@Column(name = "RUTA_ARCHIVO")
	private String rutaArchivo;
	
	@Column(name = "ARCHIVO")
	@Lob
	private String archivo;

}
