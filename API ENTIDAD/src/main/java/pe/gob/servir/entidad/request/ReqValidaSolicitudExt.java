package pe.gob.servir.entidad.request;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Data;

@Data
public class ReqValidaSolicitudExt {

	@NotNull(message = "Campo solicitudExtId es obligatorio")
    @Valid
    private Long solicitudExtId;
	
}
