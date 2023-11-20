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
public class EvaluadorEvaluadoDTO {
	
	@Id
	private String apellidosNombresEvaluado;
	private String nroDocumentoEvaluado;
	private String puestoEvaluado;
	private String segmentoEvaluado;
	private String unidadOrganicaEvaluado;
	private String entidadEvaluado;
	private String apellidosNombresEvaluador;
	private String nroDocumentoEvaluador;
	private String puestoEvaluador;
	private String segmentoEvaluador;
	private String unidadOrganicaEvaluador;
	private String entidadEvaluador;
	private String urlLogoEntidad;
	
	
}