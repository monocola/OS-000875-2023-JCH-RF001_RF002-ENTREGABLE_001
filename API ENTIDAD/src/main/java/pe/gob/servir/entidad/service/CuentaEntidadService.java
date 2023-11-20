package pe.gob.servir.entidad.service;

import java.util.Map;

import pe.gob.servir.entidad.request.ReqActualizaCuentaEntidad;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqCreaCuentaEntidad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespComboUsuarioPorEntidadRol;
import pe.gob.servir.entidad.response.RespInactivarCuenta;
import pe.gob.servir.entidad.response.RespListaEntidades;
import pe.gob.servir.entidad.response.RespReasignarCuentaEntidad;
import pe.gob.servir.entidad.response.RespValidaTarea;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface CuentaEntidadService { 
	RespBase<RespListaEntidades> buscarEntidadesAsociadas(Map<String, Object> parametroMap); 
	
	RespBase<Object> registrarCuentEnti(ReqBase<ReqCreaCuentaEntidad> request, MyJsonWebToken token);
	
	RespBase<Object> actualizarCuentaEntidad(ReqBase<ReqActualizaCuentaEntidad> request, MyJsonWebToken token, Long cuentaEntidadId);
	
	RespBase<RespInactivarCuenta> inactivarCuentaEntidad(Long cuentaId,String estado,MyJsonWebToken token);
	
	RespBase<RespValidaTarea> validaTareaCuentaEntidad(Long cuentaEntidadId);
	
	RespBase<RespReasignarCuentaEntidad> reAsignarCuentaEntidad(Long cuentaId,ReqBase<ReqCreaCuentaEntidad> request, MyJsonWebToken token);

	RespBase<RespComboUsuarioPorEntidadRol> listarCuentasPorEntidadPorRol(Map<String, Object> parametroMap);

}
