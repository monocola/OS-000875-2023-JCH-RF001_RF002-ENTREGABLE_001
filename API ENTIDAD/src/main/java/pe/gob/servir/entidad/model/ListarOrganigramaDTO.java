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
public class ListarOrganigramaDTO {
	@Id
	private Long idOrganigrama;
	private Long idEntidad;
	private Long areaId;
	private Long sedeId;
	private String puesto;
	private Long orden;
	private String descripcion;
	private Long nivelId;
	private String desNivel;
	private String sigla;
	private Long naturalezaId;
	private String desNaturaleza;
	private String estadoId;
	private String estado;
	private Long padreIdOrgHijo;
	private Long tipoOrganoId;
	private String desTipoOrgano;
	private Long nivelGobiernoId;
	private String descripcionCorta;
	private Long personaResponsableId;
	private String nombres;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private Long tipoDocumentoId;
	private String tipoDocumento;
	private String numeroDocumento;
	private Long telefonoId;
	private String telefono;
	private Long correoId;
	private String correo;
	private Long paisId;
	private String nombrePais;
	private Long padreId;

}
