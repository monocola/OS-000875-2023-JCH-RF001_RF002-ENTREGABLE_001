package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;

@Getter
@Setter
@ToString
public class RespListarSolicitudExterna {
	
	private List<SolicitudExternaDTO> solicitudExternaDto; 
	
}
