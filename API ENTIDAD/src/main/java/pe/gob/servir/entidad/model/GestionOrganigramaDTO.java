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
public class GestionOrganigramaDTO {

	@Id
	private Long organigramaId;
	private Long tipoOrganoId;
	private Long uoSuperiorId;
	private String tipoOrgano;
	private String nombreUO;
	private String siglaUO;
	private String siglaUOSup;

}
