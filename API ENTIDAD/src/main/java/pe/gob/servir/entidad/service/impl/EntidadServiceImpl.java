package pe.gob.servir.entidad.service.impl;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Strings;

import pe.gob.servir.entidad.api.dto.ApiActualizarPersonaJuridica;
import pe.gob.servir.entidad.api.dto.ApiFileServerDTO;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
//import pe.gob.servir.entidad.api.dto.ApiUploadFile;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.model.Direccion;
import pe.gob.servir.entidad.model.Entidad;
import pe.gob.servir.entidad.model.GenericoDTO;
import pe.gob.servir.entidad.model.ListaEntidadDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.ResumenServidoresCivilesDTO;
import pe.gob.servir.entidad.model.ResumenServidoresCivilesGDRDTO;
import pe.gob.servir.entidad.repository.DireccionRepository;
import pe.gob.servir.entidad.repository.EntidadRepository;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEntidad;
import pe.gob.servir.entidad.request.dto.CorreoDTO;
import pe.gob.servir.entidad.request.dto.TelefonoDTO;
import pe.gob.servir.entidad.response.RespApiFile;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespEntidad;
import pe.gob.servir.entidad.response.RespListaEntidad;
import pe.gob.servir.entidad.response.RespListaServidoresCiviles;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGDR;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGDRGraficosDonats;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGraficosDonats;
import pe.gob.servir.entidad.response.RespObtenerCorreo;
import pe.gob.servir.entidad.response.RespObtenerTelefono;
//import pe.gob.servir.entidad.response.RespUploadFile;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.EntidadService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class EntidadServiceImpl implements EntidadService {
	
	@Autowired
	private EntidadRepository entidadRepository;
	
	@Autowired
	private GeneralRepository generalRepositorySP;
	
	@Autowired
	private MaestraApiClient maestraApiClient; 
	
	@Autowired
	private PersonaApiClient personaApiClient;
	
	@Autowired
	private GestionRepository gestionRepository;
	
	@Autowired
	private DireccionRepository direccionRepository;
	
	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespEntidad> actualizarEntidad(ReqBase<ReqEntidad> request, MyJsonWebToken token,Long entidadId) {
		Optional<Entidad> entidad = entidadRepository.findById(entidadId);
		RespBase<RespEntidad> responseEntidad =  new RespBase<>();		
		RespEntidad respPayload = new RespEntidad();					
		if(entidad.isPresent()) {
			Entidad entidades = entidad.get();
			if(!Strings.isNullOrEmpty(request.getPayload().getEntidad().getDireccion())){
				validacionDireccion(request, token, entidades);	
			}			
			RespBase<RespObtenerTelefono> responseTelefono = personaApiClient.obtenerTelefonoBypersonaId(entidades.getPersonaId().intValue());		
			for (TelefonoDTO telefono : request.getPayload().getTelefonos()) {
				if(responseTelefono.getPayload().getCount() > 0) {				
					RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = new RespBase<ApiPersonaRequestDTO.Telefono>();
					ApiPersonaRequestDTO.Telefono telenoPayload = new ApiPersonaRequestDTO.Telefono(telefono.getTipoTelefono(), telefono.getCodigoArea(), telefono.getNumeroTelefono(), telefono.getNumeroAnexo());
					requestTelefono.setPayload(telenoPayload);
					RespBase<RespApiPersona.Telefono> respuesta = personaApiClient.actualizaTelefono(String.valueOf(responseTelefono.getPayload().getItems().get(0).getTelefonoId()), requestTelefono);
					if(Boolean.FALSE.equals(respuesta.getStatus().getSuccess())) {						
						return ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, respuesta.getStatus().getError().toString());
					}
				}else {
					RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = new RespBase<ApiPersonaRequestDTO.Telefono>();
					ApiPersonaRequestDTO.Telefono telenoPayload = new ApiPersonaRequestDTO.Telefono(telefono.getTelefonoId(), telefono.getTipoTelefono(), telefono.getCodigoArea(), telefono.getNumeroTelefono(), telefono.getNumeroAnexo());
					requestTelefono.setPayload(telenoPayload);
					RespBase<RespApiPersona.Telefono> respuesta = personaApiClient.crearTelefono(entidades.getPersonaId().intValue(), requestTelefono);
					if(Boolean.FALSE.equals(respuesta.getStatus().getSuccess())) {
						return ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, respuesta.getStatus().getError().toString());
					}
				}				
			}
			RespBase<RespObtenerCorreo> responseCorreo = personaApiClient.obtenerCorreoBypersonaId(entidades.getPersonaId().intValue());
			for (CorreoDTO correo : request.getPayload().getCorreos()) {
				if(responseCorreo.getPayload().getCount() > 0) {
					RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<ApiPersonaRequestDTO.Correo>();
					ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(correo.getCorreoId(),correo.getTipoCorreo(), correo.getCorreo());
					requestCorreo.setPayload(correoPayload);
					RespBase<RespApiPersona.Correo> respuesta = personaApiClient.actualizaCorreo(String.valueOf(responseCorreo.getPayload().getItems().get(0).getCorreoId()), requestCorreo);
					if(Boolean.FALSE.equals(respuesta.getStatus().getSuccess())) {
						return ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, respuesta.getStatus().getError().toString());
					}
				}else {
					RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<ApiPersonaRequestDTO.Correo>();
					ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(correo.getCorreoId(), correo.getTipoCorreo(), correo.getCorreo());
					requestCorreo.setPayload(correoPayload);
					RespBase<RespApiPersona.Correo> respuesta = personaApiClient.crearCorreo(entidades.getPersonaId().intValue(), requestCorreo);
					if(Boolean.FALSE.equals(respuesta.getStatus().getSuccess())) {
						return ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, respuesta.getStatus().getError().toString());
					}
				}
				
			} //ENDPOINT_INS_IMAGEN
			
			//Cargar logo y guardar su imagen redimensionado
			RespBase<RespApiFile> responseWS =  new RespBase<RespApiFile>();
			String rutaLogo = null;
			if(request.getPayload().getLogo().getFileBase64() != null && request.getPayload().getLogo().getFlag() == 0) {
				Parametro parametro = generalRepositorySP.buscarParametro(null,null,Constantes.RUTA_FILE_SERVER_ENTIDAD);
				String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);
				rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD, entidades.getEntidadId().toString());
				ReqBase<ApiFileServerDTO> requestApiUploadFile = new ReqBase<ApiFileServerDTO>();
				ApiFileServerDTO uploadFile = new ApiFileServerDTO();
				uploadFile.setExtension("." + ParametrosUtil.extension(request.getPayload().getLogo().getFileName()));
				uploadFile.setFileBase64(request.getPayload().getLogo().getFileBase64());
				uploadFile.setFileName(ParametrosUtil.onlyName(request.getPayload().getLogo().getFileName()));
				uploadFile.setPath(rutaFileServer);

				Parametro parametroRatioCambioLogo = generalRepositorySP.buscarParametro(null,null,Constantes.ENTIDAD_RATIO_DE_CAMBIO_LOGO);
				uploadFile.setRatioDeCambio(new Double(parametroRatioCambioLogo.getValorTexto()));
				uploadFile.setResize(true);
				requestApiUploadFile.setPayload(uploadFile);
				responseWS = maestraApiClient.insertImagen(requestApiUploadFile);
				rutaLogo = responseWS.getPayload().getPathRelative();
				entidades.setUrlLogoEntidad(rutaLogo);
				if(Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
					responseEntidad = ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, responseWS.getStatus().getError().toString());
					return responseEntidad;
				}
			
			} else {
				responseWS =  null;
			}
			
			
			//Cargar Portada y guardar su imagen redimensionado
			responseWS =  new RespBase<RespApiFile>();
			String rutaPortada = null;
			if(request.getPayload().getPortada().getFileBase64() != null && request.getPayload().getPortada().getFlag() == 0) {
				Parametro parametro = generalRepositorySP.buscarParametro(null,null,Constantes.RUTA_FILE_SERVER_ENTIDAD);//TODO revisar si es la misma ruta
				String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);
				rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD, entidades.getEntidadId().toString());
				ReqBase<ApiFileServerDTO> requestApiUploadFile = new ReqBase<ApiFileServerDTO>();
				ApiFileServerDTO uploadFile = new ApiFileServerDTO();
				uploadFile.setExtension("." + ParametrosUtil.extension(request.getPayload().getPortada().getFileName()));
				uploadFile.setFileBase64(request.getPayload().getPortada().getFileBase64());
				uploadFile.setFileName(ParametrosUtil.onlyName(request.getPayload().getPortada().getFileName()));
				uploadFile.setPath(rutaFileServer);

				Parametro parametroRatioCambioPortada = generalRepositorySP.buscarParametro(null,null,Constantes.ENTIDAD_RATIO_DE_CAMBIO_PORTADA);
				uploadFile.setRatioDeCambio(new Double(parametroRatioCambioPortada.getValorTexto()));
				uploadFile.setResize(true);
				requestApiUploadFile.setPayload(uploadFile);
				responseWS = maestraApiClient.insertImagen(requestApiUploadFile);
				rutaPortada = responseWS.getPayload().getPathRelative();
				entidades.setUrlPortadaEntidad(rutaPortada);
				if(Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
					responseEntidad = ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, responseWS.getStatus().getError().toString());
					return responseEntidad;
				}
			
			} else {
				responseWS =  null;
			}
			
			
			
			entidades.setDescripcionEntidad(request.getPayload().getEntidad().getDescripcionEntidad());
	        entidades.setNroSindicatos(request.getPayload().getEntidad().getNrosSindicatos());
			entidades.setSigla(request.getPayload().getEntidad().getSigla());
			entidades.setUrlWebEntidad(request.getPayload().getEntidad().getUrlWebEntidad());			
			entidades.setFlagActualiza(Constantes.FLAG_ACTUALIZACION_ENTIDAD);
			if(request.getPayload().getLogo().getFlag() == 1) {
				entidades.setUrlLogoEntidad(rutaLogo);
			}
			if(request.getPayload().getPortada().getFlag() == 1) {
				entidades.setUrlPortadaEntidad(rutaPortada);
			}
			entidades.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(), Instant.now());
			entidadRepository.save(entidades);
			respPayload.setEntidad(entidades);
		}else {
			responseEntidad = ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, "NO SE ENCONTRO LA ENTIDAD");			
		}	

		return new RespBase<RespEntidad>().ok(respPayload);
	}

	@SuppressWarnings("unused")
	@Override
	public RespBase<RespEntidad> actualizarEntidadGme(ReqBase<ReqEntidad> request, MyJsonWebToken token,
			Long entidadId) {

		RespEntidad respPayload = new RespEntidad();
		RespBase<RespEntidad> responseEntidad = new RespBase<>();
		try {

			Optional<Entidad> entidad = entidadRepository.findById(entidadId);

			RespBase<ApiActualizarPersonaJuridica> requestPersonaJuridica = new RespBase<>();
			ApiActualizarPersonaJuridica oApiActualizarPersonaJuridica = new ApiActualizarPersonaJuridica();
			if (entidad.isPresent()) {
				Entidad entidades = entidad.get();

				// ENDPOINT_INS_IMAGEN
				// Cargar logo y guardar su imagen redimensionado
				
				RespBase<RespApiFile> responseWS = new RespBase<RespApiFile>();
				String rutaLogo = null;

				if (request.getPayload().getLogo().getFileBase64() != null
						&& request.getPayload().getLogo().getFlag() == 0) {

					Parametro parametro = generalRepositorySP.buscarParametro(null, null,
							Constantes.RUTA_FILE_SERVER_ENTIDAD);
					
					String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto())
							.substring(1);
					rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,
							entidades.getEntidadId().toString());
					ReqBase<ApiFileServerDTO> requestApiUploadFile = new ReqBase<ApiFileServerDTO>();
					ApiFileServerDTO uploadFile = new ApiFileServerDTO();
					uploadFile
							.setExtension("." + ParametrosUtil.extension(request.getPayload().getLogo().getFileName()));
//					uploadFile.setFileBase64(request.getPayload().getLogo().getFileBase64());
					String[] array = request.getPayload().getLogo().getFileBase64().split(",");
					String base = array[array.length-1];
					uploadFile.setFileBase64(base);
					uploadFile.setFileName(ParametrosUtil.onlyName(request.getPayload().getLogo().getFileName()));
					uploadFile.setPath(rutaFileServer);
					Parametro parametroRatioCambioLogo = generalRepositorySP.buscarParametro(null, null,
							Constantes.ENTIDAD_RATIO_DE_CAMBIO_LOGO);
//					uploadFile.setRatioDeCambio(new Double(parametroRatioCambioLogo.getValorTexto()));
					uploadFile.setRatioDeCambio(Double.valueOf(parametroRatioCambioLogo.getValorTexto()));
					uploadFile.setResize(true);
					requestApiUploadFile.setPayload(uploadFile);
					responseWS = maestraApiClient.insertImagen(requestApiUploadFile);
					rutaLogo = responseWS.getPayload().getPathRelative();
					entidades.setUrlLogoEntidad(rutaLogo);
					if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
						responseEntidad = ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE,
								responseWS.getStatus().getError().toString());
						return responseEntidad;
					}

				} else {
					responseWS = null;
				}

				// Actualizar razon social
				if (request.getPayload().getActualizaRazon() == 1) {

					RespBase<RespApiPersona> responsePersona = personaApiClient
							.obtenerPersonaById(entidades.getPersonaId().intValue());

					if (responsePersona.getPayload().getPersona().getPersonaId() > 0) {
						oApiActualizarPersonaJuridica
								.setPersonaId(responsePersona.getPayload().getPersona().getPersonaId());
						oApiActualizarPersonaJuridica.setNombreComercial(
								responsePersona.getPayload().getPersonaJuridica().getNombreComercial());
						oApiActualizarPersonaJuridica.setRazonSocial(request.getPayload().getRazonSocial());
						oApiActualizarPersonaJuridica.setRuc(request.getPayload().getRuc());
						requestPersonaJuridica.setPayload(oApiActualizarPersonaJuridica);
						RespBase<RespApiPersona> respuesta = personaApiClient.actualizaPersonaJuridica(
								responsePersona.getPayload().getPersona().getPersonaId(), requestPersonaJuridica);
						if (Boolean.FALSE.equals(respuesta.getStatus().getSuccess())) {
							return ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE,
									respuesta.getStatus().getError().toString());
						}
					}
				}

				entidades.setNroSindicatos(request.getPayload().getEntidad().getNrosSindicatos());
				entidades.setSigla(request.getPayload().getEntidad().getSigla());
				entidades.setSectorId(request.getPayload().getEntidad().getSectorId() !=null ? request.getPayload().getEntidad().getSectorId().longValue() : null);
		
				entidades.setNivelGobiernoId(request.getPayload().getEntidad().getNivelGobiernoId() !=null ? request.getPayload().getEntidad().getNivelGobiernoId().longValue() : null);
				entidades.setTipoEntidadPubId(request.getPayload().getEntidad().getTipoEntidadId());
				entidades.setFlagActualiza(Constantes.FLAG_ACTUALIZACION_ENTIDAD);

				if (request.getPayload().getLogo().getFlag() == 1) {
					//entidades.setUrlLogoEntidad(rutaLogo);
					entidades.setUrlLogoEntidad("");
				}

				entidades.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),
						Instant.now());

				entidadRepository.save(entidades);
				Optional<Entidad> entidadResponse = entidadRepository.findById(entidadId);
				Entidad entidadesResponse = entidad.get();
				respPayload.setEntidad(entidadesResponse);
			} else {
				responseEntidad = ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE,
						"NO SE ENCONTRO LA ENTIDAD");
			}
		} catch (Exception e) {
			// TODO: handle exception
			return ParametrosUtil.setearResponse(responseEntidad, Boolean.FALSE, e.getMessage());
		}
		return new RespBase<RespEntidad>().ok(respPayload);
	}

	private void validacionDireccion(ReqBase<ReqEntidad> request, MyJsonWebToken token, Entidad entidades) {
		Direccion direccion = null;
		if(entidades.getDireccionId() != null) {
			Optional<Direccion> direccionFind = direccionRepository.findById(entidades.getDireccionId());
			if(direccionFind.isPresent()) {
				direccion = direccionFind.get();
				direccion.setDireccion(request.getPayload().getEntidad().getDireccion());
				direccion.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),
						Instant.now());
				direccionRepository.save(direccion);
				entidades.setDireccionId(direccion.getDireccionId());
			}					
		}else {
			direccion = new Direccion();
			direccion.setUbigeoId(null);
			direccion.setDireccion(request.getPayload().getEntidad().getDireccion());
			direccion.setZonaGeografica(null);
			direccion.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			direccionRepository.save(direccion);
			entidades.setDireccionId(direccion.getDireccionId());
		}
	}
	
	@Override
	public RespBase<RespListaEntidad> listarEntidad(Long entidadId) {
		List<ListaEntidadDTO> listaEntidades = gestionRepository.listaEntidad(entidadId);
		RespListaEntidad respPayload = new RespListaEntidad();
		respPayload.setEntidad(listaEntidades);
		return new RespBase<RespListaEntidad>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaEntidad> listarEntidades() {
		List<ListaEntidadDTO> listaEntidades = gestionRepository.listarEntidades();
		RespListaEntidad respPayload = new RespListaEntidad();
		respPayload.setEntidad(listaEntidades);
		return new RespBase<RespListaEntidad>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaServidoresCiviles> resumenDeServidoresCiviles(Long entidadId) {
		List<ResumenServidoresCivilesDTO> listaEntidades = gestionRepository.resumenDeServidoresCiviles(entidadId);
		RespListaServidoresCiviles respPayload = new RespListaServidoresCiviles();
		respPayload.setServidoresCiviles(listaEntidades);
		return new RespBase<RespListaServidoresCiviles>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaServidoresCivilesGDR> resumenDeServidoresCivilesGDR(Long entidadId) {
		List<ResumenServidoresCivilesGDRDTO> listaEntidades = gestionRepository
				.resumenDeServidoresCivilesGDR(entidadId);
		RespListaServidoresCivilesGDR respPayload = new RespListaServidoresCivilesGDR();
		respPayload.setServidoresCivilesGDR(listaEntidades);
		return new RespBase<RespListaServidoresCivilesGDR>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaServidoresCivilesGraficosDonats> resumenDeServidoresCivilesGraficosDonats(Long entidadId) {
		List<GenericoDTO> servidoresTipoOrgano = gestionRepository.resumenDeServidoresCivilesTipoOrgano(entidadId);
		RespListaServidoresCivilesGraficosDonats respPayload = new RespListaServidoresCivilesGraficosDonats();
		List<GenericoDTO> servidoresRegiLaboral = gestionRepository.resumenDeServidoresCivilesRegimenLaboral(entidadId);
		respPayload.setServidoresCivilesRegimenLaboral(servidoresRegiLaboral);
		respPayload.setServidoresCivilesTipoOrgano(servidoresTipoOrgano);
		return new RespBase<RespListaServidoresCivilesGraficosDonats>().ok(respPayload);
	}

	@SuppressWarnings("unchecked")
	@Override
	public RespBase<RespListaServidoresCivilesGDRGraficosDonats> resumenDeServidoresCivilesGDRGraficosDonats(
			Long entidadId) {
		RespListaServidoresCivilesGDRGraficosDonats respPayload = new RespListaServidoresCivilesGDRGraficosDonats();
		List<GenericoDTO> servidoresTipoOrganoGDR = gestionRepository
				.resumenDeServidoresCivilesTipoOrganoGDR(entidadId);
		List<GenericoDTO> servidoresRegiLaboralGDR = gestionRepository
				.resumenDeServidoresCivilesRegimenLaboralGDR(entidadId);
		List<GenericoDTO> servidoresCarrerasEspecialesGDR = gestionRepository
				.resumenDeServidoresCivilesCarrerasEspecialesGDR(entidadId);
		List<GenericoDTO> servidoresPorSegmentoGDR = gestionRepository
				.resumenDeServidoresCivilesPorSegmentoGDR(entidadId);
		List<GenericoDTO> servidoresPorSindicalizadosGDR = gestionRepository
				.resumenDeServidoresCivilesSindicalizadosGDR(entidadId);

		List<GenericoDTO> servidoresPorSindicalizadosGDRAgr = new ArrayList<GenericoDTO>();

		servidoresPorSindicalizadosGDR.stream()
				.collect(Collectors.groupingBy(foo -> foo.codigoTexto,
						Collectors.summingInt(foo -> Integer.parseInt(foo.descripcion))))
				.forEach((codigoTexto, descripcion) -> {
					GenericoDTO oGenericoDTO = new GenericoDTO();
					oGenericoDTO.setGraficoid(1);
					oGenericoDTO.setCodigoTexto(codigoTexto);
					oGenericoDTO.setDescripcion(String.valueOf(descripcion));
					servidoresPorSindicalizadosGDRAgr.add(oGenericoDTO);
				});

		respPayload.setServidoresCivilesCarrerasEspecialesGdr(servidoresCarrerasEspecialesGDR);
		respPayload.setServidoresCivilesPorSegmentoGdr(servidoresPorSegmentoGDR);
		respPayload.setServidoresCivilesRegimenLaboralGdr(servidoresRegiLaboralGDR);
		respPayload.setServidoresCivilesSindicalizadosGdr(servidoresPorSindicalizadosGDRAgr);
		respPayload.setServidoresCivilesTipoOrganoGdr(servidoresTipoOrganoGDR);
		return new RespBase<RespListaServidoresCivilesGDRGraficosDonats>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaEntidad> listarEntidadesPorListId(String listId) {
		List<ListaEntidadDTO> listaEntidades = gestionRepository.listarEntidadesPorListId(listId);
		RespListaEntidad respPayload = new RespListaEntidad();
		respPayload.setEntidad(listaEntidades);
		return new RespBase<RespListaEntidad>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaEntidad> listarEntidadFilter(Map<String, Object> parametroMap) {
		List<ListaEntidadDTO> lista = gestionRepository.listaEntidadFilt(parametroMap);
		RespListaEntidad respPayload = new RespListaEntidad();
		respPayload.setEntidad(lista);
		return new RespBase<RespListaEntidad>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaEntidad> listarEntidadPorSigla(String sigla, boolean soloActivos) {
		List<Entidad> listaEntidades = entidadRepository.findBySigla(sigla.toUpperCase());
		List<ListaEntidadDTO> listaEntidadesDTO = new ArrayList<ListaEntidadDTO>();
		for (Entidad entidad : listaEntidades) {
			if (!soloActivos || (soloActivos && entidad.getEstadoRegistro().equals("1"))) {
				ListaEntidadDTO lista = new ListaEntidadDTO();
				lista.setEntidadId(entidad.getEntidadId().intValue());
				lista.setPersonaId(entidad.getPersonaId().intValue());
				lista.setDescripcionEntidad(entidad.getDescripcionEntidad());
				lista.setSigla(entidad.getSigla());
				listaEntidadesDTO.add(lista);
			}
		}
		RespListaEntidad respPayload = new RespListaEntidad();
		respPayload.setEntidad(listaEntidadesDTO);
		return new RespBase<RespListaEntidad>().ok(respPayload);
	}
	
}
