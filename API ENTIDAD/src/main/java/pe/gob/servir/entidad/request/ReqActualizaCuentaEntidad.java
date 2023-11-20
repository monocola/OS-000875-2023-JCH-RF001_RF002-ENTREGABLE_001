package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.request.dto.CorreoDTO;
import pe.gob.servir.entidad.request.dto.CuentaEntidadDTO;
import pe.gob.servir.entidad.request.dto.TelefonoDTO;

@Getter
@Setter
public class ReqActualizaCuentaEntidad {
	@NotNull(message = "Campo listaRoles es obligatorio")
	@Valid
	private CuentaEntidadDTO cuentaEntidad;
	
	@Valid
	private TelefonoDTO telefono;
	
	@Valid
	private CorreoDTO correo;
	
	@NotNull(message = "Campo listaRoles es obligatorio")
	@Valid
	private List<AsignaRolRequestDTO> listaRoles; 

}
