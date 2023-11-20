package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.model.ObtenerCuentaEntidadRolDTO;

@Getter
@Setter
@ToString
public class RespComboUsuarioPorEntidadRol {

	private List<ObtenerCuentaEntidadRolDTO> items;

}
