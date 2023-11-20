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
public class UnidadOrganicaDTO {
	
	@Id
	private Long organigramaId;

	private Long entidadId;

	private Long areaId;	
	
	private Long sedeId;
	
	private String puesto;
	
	private Integer orden;
	
	private Long nivel;
	
	private String desNivel;
	
	private Long tipoOrganoUoId;
	
	private String descripcionTipoOrg;
	
	private Long nivelGobiernoId;
	
	private String unidadOrganica;
	
	private String descripcionCorta;
	
	private String sigla;
	
	private String estadoRegistro;
	
	private String estado; 
	
	private Long personaResponsableId;
	
	private String nombres;
	
	private String apellidoPaterno;
	
	private String apellidoMaterno;
	
	private Long telefonoId;
	
	private String telefono;
	
	private Long correoId;	
	
	private String correo; 
	
	private Integer tipoDocumento;
	
	private String destipoDocumento;
	
	private String nroDocumento;

	private Long padreOrganigramaId;	
	
	private String descripOrganoPadre;
	
	private Long paisId;
	
	private String nombrePais;
	
}
