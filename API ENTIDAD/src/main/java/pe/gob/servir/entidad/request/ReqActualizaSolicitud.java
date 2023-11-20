package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.request.dto.SolicitudArchivoDTO;
import pe.gob.servir.entidad.request.dto.SolicitudDTO;
import pe.gob.servir.entidad.request.dto.SolicitudPersonaDTO;

@Getter
@Setter
public class ReqActualizaSolicitud {
	
	@NotNull(message = "Campo solicitud es obligatorio")
	@Valid
	private SolicitudDTO solicitud;
	
	@Valid
	private List<SolicitudArchivoDTO> listaSolicitudAdjunto;
	
	@Valid
	private List<SolicitudPersonaDTO>  listaSolicitudPersona;
}
