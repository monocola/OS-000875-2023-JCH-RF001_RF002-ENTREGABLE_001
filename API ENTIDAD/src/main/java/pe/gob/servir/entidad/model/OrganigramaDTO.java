package pe.gob.servir.entidad.model;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class OrganigramaDTO {	
	@Id
	private Long rowNum;
	private Long personaResponsableId;
	private String nombres;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private Long telefonoId;	
	private String telefono;
	private Long correoId;
	private String correo;
	private Integer tipoDocumento;
	private String nroDocumento;
	private Long organigramaId;

	private Long entidadId;

	private Long areaId;
	
	private Long paisId;
	
	private Long sedeId;

	private Long padreOrganigramaId;

	private String puesto;

	private Integer orden;

	private Long nivel;

	private Long tipoOrganoUoId;

	private Long naturalezaOrgano;

	private Long nivelGobiernoId;

	private String descripcion;

	private String descripcionCorta;

	private String sigla;	

	private String descripcionTipoOrg;
	private String estado;
	
	private String descripOrganoPadre;
	
	private String nombrePais;
	private String desNivel;
	private String desNaturaleza;
	private String estadoRegistro;
	private String destipoDocumento;
	private String urlFoto;
	
	public OrganigramaDTO() {
		super();
		// TODO Auto-generated constructor stub
	}

	
	
	
	
}
