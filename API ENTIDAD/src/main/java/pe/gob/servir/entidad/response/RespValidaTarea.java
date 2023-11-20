package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ValidaTareaCuentaEntidad;

@Getter
@Setter
public class RespValidaTarea {
	private List<ValidaTareaCuentaEntidad> tareaCuentaEntidad;

}
