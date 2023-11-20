package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class BeanServidorCivilDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@NotNull
	@Valid
	private ServidorCivilGDRDTO servidorCivil;
	private List<RolesDTO> roles;
	private Long entidadId;
	private String excluye;	
}