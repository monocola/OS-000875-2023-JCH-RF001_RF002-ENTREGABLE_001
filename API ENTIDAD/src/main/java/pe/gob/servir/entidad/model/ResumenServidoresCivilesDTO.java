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
public class ResumenServidoresCivilesDTO {
	@Id	
	private Integer entidadId;
	private Integer totalEntidad;
	private Integer totalCarrerasEspeciales;
	private Integer totalSindicalizados;
	private Integer totalResponsablesOOUSUBOU;
	private Integer totalModalidadFormativaSimilares;
	private Integer totalEncargadosOOUSUBOU;

}
