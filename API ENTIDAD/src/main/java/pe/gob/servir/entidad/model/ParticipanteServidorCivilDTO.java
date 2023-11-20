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
public class ParticipanteServidorCivilDTO {

	@Id
	private Long detUnidadOrganicaId;
	private Long personaId;
	private String documentoIdentidad;
	private String numeroDocumento;
	private String apellidosNombres;
	private Long unidadOrganicaId;
	private String esJefeUo;
	private String puesto;
	private Long segmentoId;
	private String segmento;
	private Long rolId;
	private String descripcionRoles;
	private Long estadoSerCivGdrId;
	private String siglaUO;
	private String estado;
	private Integer indicadorMeta;
	private String tipoAsignacion;

}