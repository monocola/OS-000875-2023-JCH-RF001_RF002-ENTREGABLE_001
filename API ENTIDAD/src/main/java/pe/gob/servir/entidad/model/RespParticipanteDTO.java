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
public class RespParticipanteDTO {

	@Id
	private Long detUnidadOrganicaId;
	private Long uoId;
	private String areaAbreviado;
	private String apellidosNombres;
	private Long puestoId;
	private String puesto;
	private String segmento;
	private String segmentoCorto;
	private String descripcionRoles;
	private String urlFoto;
	private Boolean flagEvaluador;
	private String esJefe;
	private String esResponsable;
	private Long tipoAsignacion;
	private Long uoPadreId;

}