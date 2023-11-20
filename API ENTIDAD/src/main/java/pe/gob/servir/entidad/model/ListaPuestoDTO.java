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
public class ListaPuestoDTO {
	
	@Id
	private String puestoId;
	private String entidadId;
	private String uoId;
	private String unidadOrganica;
	private String nombrePuesto;
	private String jefeInmediato;
	private String esJefe;
	private String nroTrabajadores;

}
