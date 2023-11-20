package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.request.dto.CuentaEntidadDTO;
import pe.gob.servir.entidad.request.dto.PersonaNaturalDTO;

@Getter
@Setter
public class ReqCreaCuentaEntidad {
	@NotNull(message = "Campo cuentaEntidad es obligatorio")
	@Valid
	private CuentaEntidadDTO cuentaEntidad;
	
	@Valid
	private PersonaNaturalDTO personaNatural;
	
	@NotNull(message = "Campo listaRoles es obligatorio")
	@Valid
	private List<AsignaRolRequestDTO> listaRoles; 
	
	

}
