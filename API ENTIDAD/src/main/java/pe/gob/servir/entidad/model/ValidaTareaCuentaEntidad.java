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
public class ValidaTareaCuentaEntidad {
	@Id
	private Integer cuentaId;
	private String flagTareaPendiente;
	

}
