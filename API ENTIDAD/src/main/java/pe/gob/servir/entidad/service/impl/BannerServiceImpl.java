package pe.gob.servir.entidad.service.impl;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import pe.gob.servir.entidad.api.dto.ApiFileServerDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.model.Banner;
import pe.gob.servir.entidad.model.ListaBannersDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.repository.BannerRepository;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.response.RespApiFile;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespListaBanners;
import pe.gob.servir.entidad.response.RespMensaje;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.BannerService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class BannerServiceImpl implements BannerService{
	
	
	@Autowired
	private GeneralRepository generalRepositorySP;
	
	@Autowired
	private MaestraApiClient maestraApiClient;
	
	@Autowired
	BannerRepository bannerRepository;
	
	@Autowired
	VariablesSistema variablesSistema;
	
	private String bannerNoExiste="Banner Id no existe.";
	private String mensajeerror="Ocurrio un error.";

	@Override
	public RespBase<RespMensaje> guardarBanner(MultipartFile file,String urlWeb, MyJsonWebToken token) {
		RespMensaje payload = new RespMensaje();
		String mensaje="";
		Long bannerId=0L;
		List<String> files = new ArrayList<>();

		try {

			
			
				if(!file.isEmpty()) {
					Parametro parametro = generalRepositorySP.buscarParametro(null,null,Constantes.RUTA_FILE_SERVER_SERVIR);
					Parametro parametroRatioCambioPortada = generalRepositorySP.buscarParametro(null,null,Constantes.ENTIDAD_RATIO_DE_CAMBIO_PORTADA);
					
					String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);
					ReqBase<ApiFileServerDTO> requestApiUploadFile = new ReqBase<>();

						Banner banner = new Banner();
						
						ApiFileServerDTO uploadFile = new ApiFileServerDTO();
						uploadFile.setExtension("." + ParametrosUtil.extension(file.getOriginalFilename()));
						uploadFile.setFileBase64(Base64.getEncoder().withoutPadding().encodeToString(file.getBytes()));
						uploadFile.setFileName(ParametrosUtil.onlyName(file.getOriginalFilename()));
						uploadFile.setPath(rutaFileServer);
						
						uploadFile.setRatioDeCambio(Double.parseDouble(parametroRatioCambioPortada.getValorTexto()));
						uploadFile.setResize(true);
						
						requestApiUploadFile.setPayload(uploadFile);
						RespBase<RespApiFile> responseWS = maestraApiClient.insertImagen(requestApiUploadFile);
						 
						banner.setUrlImg(responseWS.getPayload().getPathRelative());
						String filename= file.getOriginalFilename().toLowerCase();
						if (filename.indexOf('.') > -1) { filename = filename.substring(0, filename.lastIndexOf('.')); }
						banner.setDescripcion(filename);
						banner.setUrlWeb(urlWeb);
						banner.setOrden(bannerRepository.obtenerOrden()+1);
						banner.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
						
						bannerRepository.save(banner);
					 
						files.add(file.getOriginalFilename());
						
						bannerId=banner.getBannerId();
						mensaje="Se guardó la imagen "+filename + " con éxito.";	
				
					
					
				}else {
					mensaje="No existe ninguna imagen cargada";
				}	

			
		}
		catch(Exception e) {
			
			String error=e.getMessage();
			if(error.lastIndexOf("]}}") >0){
				if(!files.isEmpty()) {
					mensaje="Ocurrio un error. "+error.substring((error.lastIndexOf(":[")+3),(error.lastIndexOf("]}}")-1))+ "Solo se guardaron las siguientes imagenes: " +
							files.stream().collect(Collectors.joining(", "));
				}else {
					mensaje="Ocurrio un error al guardar el/los banner/s. "+error.substring((error.lastIndexOf(":[")+3),(error.lastIndexOf("]}}")-1));
				
				}
			}else {
				mensaje="Ocurrio un error: "+e.getMessage(); 
			}
			
		}
		payload.setBannerId(bannerId);
		payload.setMensaje(mensaje);
		return new RespBase<RespMensaje>().ok(payload);
	}

	@Override
	public RespBase<RespListaBanners> obtenerBanners() {
       List<Banner> lista=	bannerRepository.obtenerBanners();
       List<ListaBannersDTO> banners = new ArrayList<>();
       RespListaBanners payload = new RespListaBanners();
     
       for (int i = 0; i < lista.size(); i++) {
    	   ListaBannersDTO b = new ListaBannersDTO();
    	   b.setBannerId(lista.get(i).getBannerId());
    	   b.setOrden(lista.get(i).getOrden());
    	   b.setDescripcion(lista.get(i).getDescripcion());
    	   b.setUrlImg(variablesSistema.fileServer+lista.get(i).getUrlImg());
    	   b.setUrlWeb(lista.get(i).getUrlWeb());
    	   banners.add(b);
	}
       
       payload.setBanners(banners);
		return new RespBase<RespListaBanners>().ok(payload);
	}


	@Override
	public RespBase<RespMensaje> actualizarBannerURL(Long bannerId,String urlWeb, MyJsonWebToken token) {
		 Optional<Banner> find = bannerRepository.findById(bannerId);
		 RespMensaje payload = new RespMensaje();
		 String mensaje="";
		 try {
			 if(find.isPresent()) {
				 	Banner banner = find.get();
				 	
				 	banner.setUrlWeb(urlWeb);
				 	banner.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(),token.getUsuario().getUsuario(), Instant.now());
				 	bannerRepository.save(banner);
				 	mensaje= "Se actualizó el link.";
			 }else {
				 mensaje=bannerNoExiste;
			 }
		 }
		 catch(Exception e) {
			 mensaje=mensajeerror;
		 }
		 payload.setMensaje(mensaje);
		return new RespBase<RespMensaje>().ok(payload);
	}

	@Override
	public RespBase<RespMensaje> inactivarBanner(Long bannerId, MyJsonWebToken token) {
		 Optional<Banner> find = bannerRepository.findById(bannerId);
		 RespMensaje payload = new RespMensaje();
		 String mensaje="";
		 try {
			 if(find.isPresent()) {
				 	Banner banner = find.get();
				 	
				 	banner.setCampoSegUpd(EstadoRegistro.INACTIVO.getCodigo(),token.getUsuario().getUsuario(), Instant.now());
				 	bannerRepository.save(banner);
				 	mensaje= "Se eliminó imagen.";
			 }else {
				 mensaje=bannerNoExiste;
			 }
		 }
		 catch(Exception e) {
			 mensaje=mensajeerror;
		 }
		 payload.setMensaje(mensaje);
		return new RespBase<RespMensaje>().ok(payload);
	}

	@Override
	public RespBase<RespMensaje> actualizarBannerOrden(Long bannerId, Long orden, MyJsonWebToken token) {
		 Optional<Banner> find = bannerRepository.findById(bannerId);
		 RespMensaje payload = new RespMensaje();
		 String mensaje="";
		 try {
			 if(find.isPresent()) {
				 	Banner banner = find.get();

				 	banner.setOrden(orden);
				 	banner.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(),token.getUsuario().getUsuario(), Instant.now());
				 	bannerRepository.save(banner);
				 	mensaje= "Se actualizó el orden.";
			 }else {
				 mensaje=bannerNoExiste;
			 }
		 }
		 catch(Exception e) {
			 mensaje=mensajeerror;
		 }
		 payload.setMensaje(mensaje);
		return new RespBase<RespMensaje>().ok(payload);
	}

}
