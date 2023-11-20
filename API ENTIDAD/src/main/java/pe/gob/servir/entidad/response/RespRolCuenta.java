package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ObtenerRolDTO;
@Getter
@Setter
public class RespRolCuenta {
	private List<ObtenerRolDTO> roles;

}
