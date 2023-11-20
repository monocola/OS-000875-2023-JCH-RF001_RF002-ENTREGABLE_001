package pe.gob.servir.entidad.service;

import java.io.IOException;
import java.util.Map;

import pe.gob.servir.entidad.model.SolicitudPersona;
import pe.gob.servir.entidad.request.ReqActualizaSolicitud;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqCreaSolicitud;
import pe.gob.servir.entidad.request.ReqInsObservacion;
import pe.gob.servir.entidad.request.dto.UbicacionPersonaDTO;
import pe.gob.servir.entidad.response.RespActualizaSolicitud;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespCreaSolicitud;
import pe.gob.servir.entidad.response.RespSolicitudDuplicado;
import pe.gob.servir.entidad.response.RespObtenerCorreo;
import pe.gob.servir.entidad.response.RespObtieneSolicitud;
import pe.gob.servir.entidad.response.RespObtieneSolicitudEntidad;
import pe.gob.servir.entidad.response.RespObtieneSolicitudEntidadById;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface SolicitudService {
	
	RespBase<RespCreaSolicitud> creaSolicitudEntidad(ReqBase<ReqCreaSolicitud> request, MyJsonWebToken token) throws IOException;
	
	RespBase<RespSolicitudDuplicado> validarSolicitudDuplicado(Long ruc, Long dni);
	
	RespBase<RespObtieneSolicitudEntidadById> obtieneSolicitudEntidadById(Long solicitudEntidadId);
	
	RespBase<RespObtieneSolicitudEntidad> obtieneSolicitudEntidad();	
	
	RespBase<RespObtieneSolicitud> buscarSolicituByFilter(Map<String, Object> parametroMap);

	RespBase<RespActualizaSolicitud> actualizarSolicitud(ReqBase<ReqActualizaSolicitud> request, MyJsonWebToken token,Long solicitudId);
	 
	RespBase<Object> darAltaSolicitud(MyJsonWebToken token,Long solicitudId);

	RespBase<Object> registrarObservacion(ReqBase<ReqInsObservacion> listaObs, MyJsonWebToken token,Long solicitudId);

	RespBase<RespObtieneSolicitudEntidadById> validarcodActualizacionSolicitud(String codigo, Long solicitudId);
	
	RespBase<SolicitudPersona> actualizarUbicacionPersona(ReqBase<UbicacionPersonaDTO> request, MyJsonWebToken token,Long solicitudPersonaId);
	
	RespBase<RespObtenerCorreo> validarCorreoSolicitud(String correo);
	
	RespBase<RespObtenerCorreo> notificarCorreoCambioPass(String correo);


}
