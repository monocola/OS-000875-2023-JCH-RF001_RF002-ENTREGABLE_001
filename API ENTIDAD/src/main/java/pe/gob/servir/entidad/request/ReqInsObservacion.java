package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ReqInsObservacion {
		
	@NotNull(message = "debe ingresar al menos una Observacion")
	@Valid
	List<Long> listaIdObservacion;
}
