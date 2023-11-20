package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.api.dto.ApiActualizarRolUsuario;
import pe.gob.servir.entidad.model.CuentaEntidad;

@Getter
@Setter
public class RespInactivarCuenta {
	private CuentaEntidad cuentaEntidad;
	private List<ApiActualizarRolUsuario> rolUsuarios;

}
