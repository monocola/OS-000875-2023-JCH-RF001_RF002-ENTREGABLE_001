package pe.gob.servir.entidad.model;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PuestoDatosDTO {

	private Long unidadOrganicaId;

	private String puesto;

	private String responsable;

	private String observacion;

	private String observacionResultado;

}
