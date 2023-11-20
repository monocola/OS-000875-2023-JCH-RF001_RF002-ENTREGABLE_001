package pe.gob.servir.entidad.service;

import org.springframework.web.multipart.MultipartFile;

import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespListaBanners;
import pe.gob.servir.entidad.response.RespMensaje;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface BannerService {
	RespBase<RespMensaje> guardarBanner(MultipartFile file,String urlWeb, MyJsonWebToken token);
	
	RespBase<RespListaBanners> obtenerBanners();
	
	RespBase<RespMensaje> actualizarBannerURL(Long bannerId,String urlWeb, MyJsonWebToken token);
	
	RespBase<RespMensaje> actualizarBannerOrden(Long bannerId, Long orden, MyJsonWebToken token);
	
	RespBase<RespMensaje> inactivarBanner(Long bannerId, MyJsonWebToken token);
}
