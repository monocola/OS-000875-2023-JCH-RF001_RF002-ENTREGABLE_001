package pe.gob.servir.entidad.request;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.Empleado;
import pe.gob.servir.entidad.request.dto.LogoDTO;

@Getter
@Setter
public class ReqEmpleado {

	@NotNull(message = "Campo empleado es obligatorio")
	@Valid
	private Empleado empleado;
	
	@Valid
	private LogoDTO logo;
	
	
}
