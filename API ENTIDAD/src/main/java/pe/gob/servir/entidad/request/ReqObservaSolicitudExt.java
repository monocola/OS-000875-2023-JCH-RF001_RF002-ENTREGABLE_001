package pe.gob.servir.entidad.request;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Data;

@Data
public class ReqObservaSolicitudExt {

	@NotNull(message = "Campo solicitudObs es obligatorio")
    @Valid
    private String solicitudObs;
	
	@NotNull(message = "Campo solicitudExtId es obligatorio")
    @Valid
    private Long solicitudExtId;
	
}
