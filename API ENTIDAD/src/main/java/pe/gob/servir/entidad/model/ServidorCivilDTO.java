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
public class ServidorCivilDTO {

	@Id
	private Long detUnidadOrganicaId;
	private Integer flagHabilitar;
	private Long organigramaId;
	private Long personaId;
	private String docEntidadId;
	private String apellidosNombres;
	private String unidadOrganica;
	private String puesto;
	private String tipoAsignacion;
	private Integer segmentoId;
	private Long rolId;
	private String estado;
	private String estadoRegistro;
	private Long regimenId;
}
