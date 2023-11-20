package pe.gob.servir.entidad.service;

import java.util.List;
import java.util.Map;

import org.springframework.web.multipart.MultipartFile;
import pe.gob.servir.entidad.model.ComboDTO;
import pe.gob.servir.entidad.model.Encrypt;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqObservaSolicitudExt;
import pe.gob.servir.entidad.request.ReqRegistrarSolicitudExterna;
import pe.gob.servir.entidad.request.ReqValidaSolicitudExt;
import pe.gob.servir.entidad.response.*;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface SolicitudExternaService {

	RespBase<Object> buscarSolicitudEntidadExt(Integer tipoDocumento, String numeroDocumento);
	RespBase<RespRegistrarSolicitudExterna> registrarSolicitudExterna (MyJsonWebToken jwt, ReqBase<ReqRegistrarSolicitudExterna> request);
	RespBase<RespRegistrarSolicitudExterna> actualizarSolicitudExterna (MyJsonWebToken jwt, ReqBase<ReqRegistrarSolicitudExterna> request);
	RespBase<RespListarSolicitudExterna> filtrarSolicitudExterna(Map<String, Object> parametroMap);
	RespBase<SolicitudExternaDTO> getSolicitudExternaId(Long solicitudExternaId);
	RespBase<Encrypt> getEncryptSolicitudExternaId(Long solicitudExternaId);
	RespBase<List<ComboDTO>> getAnioSolExt();
	RespBase<Object> aceptarSolicitudExterna (MyJsonWebToken token, Long solicitudExtId, ReqBase<ReqValidaSolicitudExt> request);
	RespBase<Object> rechazarSolicitudExterna (MyJsonWebToken token, Long solicitudExtId);
	RespBase<Object> observarSolicitudExterna (MyJsonWebToken token, Long solicitudExtId, ReqBase<ReqObservaSolicitudExt> request);

	RespBase<RespRespSubirArchivoNgnx> guardarArchivosNgnx(MultipartFile archivo, Map<String, String> request) throws Exception;
}
