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
public class ParticipanteEvaluadoresServidorCivilDTO {

	@Id
	private Long detUnidadOrganicaId;
	private Long uoId;
	private Long personaId;
	private String apellidosNombres;
	private Long puestoId;
	private String puesto;
	private Long segmentoId;
	private String segmento;
	private Long rolId;
	private String descripcionRoles;
	private String urlFoto;
	private Integer cantidadEvaluados;
	private Integer personaEvaluadorId;
	private Integer indicadorMeta;
	

}