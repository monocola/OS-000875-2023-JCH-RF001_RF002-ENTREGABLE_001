package pe.gob.servir.entidad.service;

import java.io.InputStream;
import java.util.Map;

import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqRegistrarPuesto;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespComboPuesto;
import pe.gob.servir.entidad.response.RespListarDetalleUO;
import pe.gob.servir.entidad.response.RespListarPuesto;
import pe.gob.servir.entidad.response.RespPuesto;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface PuestoService {


	RespBase<RespComboPuesto> buscarPuestoPorEntidad(Long entidadId,Long organigramaId);
	
	RespBase<RespComboPuesto> filtrarPuestosPorDescripcion(Map<String, Object> parametroMap);

	RespBase<Object> obtenerListaPuestosfromExcel(InputStream uploadedInputStream, Long entidadId, MyJsonWebToken jwt);

	RespBase<RespPuesto> registrarPuesto(ReqBase<ReqRegistrarPuesto> request, MyJsonWebToken jwt);
	
	RespBase<Object> editarPuesto(ReqBase<ReqRegistrarPuesto> request, MyJsonWebToken token, Long puestoId);
	
	RespBase<RespListarPuesto> listarPuesto(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespListarDetalleUO> listarPuestosUOxEvaluador(Map<String, Object> parametroMap) throws ValidationException;

	RespBase<RespListarDetalleUO> listarPuestosUOxEvaluadorGDR(Map<String, Object> parametroMap) throws ValidationException;

	RespBase<Object> descargarExcelPuesto(Long idEntidad);
}
