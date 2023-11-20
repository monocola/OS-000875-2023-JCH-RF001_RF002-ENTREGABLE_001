package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class AsignaRolRequestDTO {
	
	private Long usuarioRolId;
	
	private Long usuarioId;
	
	private Long rolId;
	
	private Long entidadId;	
}
