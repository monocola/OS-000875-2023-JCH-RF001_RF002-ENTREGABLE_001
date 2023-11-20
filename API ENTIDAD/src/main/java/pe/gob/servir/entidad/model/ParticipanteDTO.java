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
public class ParticipanteDTO {
	
	@Id
	private String apellidosNombresParticipante;
	private String urlLogoEntidad;
	private String nroDocumentoParticipante;
	private String puestoParticipante;
	private String segmentoParticipante;
	private String unidadOrganicaParticipante;
	private String entidadParticipante;
	
}