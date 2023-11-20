package pe.gob.servir.entidad.service.impl;

import java.io.IOException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

import org.apache.logging.log4j.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.googlecode.jmapper.JMapper;

import org.springframework.web.multipart.MultipartFile;
import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiActualizarRolUsuario;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.ApiSeguridadRequestDTO;
import pe.gob.servir.entidad.api.dto.ApiUploadFile;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.Archivo;
import pe.gob.servir.entidad.model.Cargo;
import pe.gob.servir.entidad.model.CorreoApiDTO;
import pe.gob.servir.entidad.model.CuentaEntidad;
import pe.gob.servir.entidad.model.CuentaEntidadDTO;
import pe.gob.servir.entidad.model.CuentaEntidadRolDTO;
import pe.gob.servir.entidad.model.Entidad;
import pe.gob.servir.entidad.model.ObtenerSolicitud;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.Solicitud;
import pe.gob.servir.entidad.model.SolicitudArchivo;
import pe.gob.servir.entidad.model.SolicitudPersona;
import pe.gob.servir.entidad.model.SolicitudRevision;
import pe.gob.servir.entidad.model.SolicitudRevisionDetalle;
import pe.gob.servir.entidad.model.ValidacionEntidad;
import pe.gob.servir.entidad.repository.ArchivoRepository;
import pe.gob.servir.entidad.repository.CargoRepository;
import pe.gob.servir.entidad.repository.CorreoRepositorySP;
import pe.gob.servir.entidad.repository.CuentaEntidadRepository;
import pe.gob.servir.entidad.repository.EntidadRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.SolicitudArchivoRepository;
import pe.gob.servir.entidad.repository.SolicitudPersonaRepository;
import pe.gob.servir.entidad.repository.SolicitudRepository;
import pe.gob.servir.entidad.repository.SolicitudRepositorySP;
import pe.gob.servir.entidad.repository.SolicitudRevisionDetalleRepository;
import pe.gob.servir.entidad.repository.SolicitudRevisionRepository;
import pe.gob.servir.entidad.repository.UsuarioRepositorySP;
import pe.gob.servir.entidad.repository.ValidacionEntidadRepository;
import pe.gob.servir.entidad.request.ReqActualizaSolicitud;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqCreaSolicitud;
import pe.gob.servir.entidad.model.EntidadDTO;
import pe.gob.servir.entidad.model.EstadoSolicitud;
import pe.gob.servir.entidad.request.ReqInsObservacion;
import pe.gob.servir.entidad.request.dto.SolicitudArchivoDTO;
import pe.gob.servir.entidad.request.dto.SolicitudDTO;
import pe.gob.servir.entidad.request.dto.SolicitudPersonaDTO;
import pe.gob.servir.entidad.request.dto.UbicacionPersonaDTO;
import pe.gob.servir.entidad.response.*;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.NotificacionService;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.service.SolicitudService;
import pe.gob.servir.entidad.util.FilesUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;
import pe.gob.servir.entidad.repository.GeneralRepository;

@Service
public class SolicitudServiceImpl implements SolicitudService {

	@Autowired
	private SolicitudRepository solicitudRepository;

	@Autowired
	private SolicitudArchivoRepository solicitudArchivoRepository;

	@Autowired
	private SolicitudPersonaRepository solicitudPersonaRepository;

	@Autowired
	private MaestraApiClient maestraApiClient;

	@Autowired
	private SolicitudRepositorySP solicitudRepositorySP;

	@Autowired
	private NotificacionService notificacionService;

	@Autowired
	private PersonaService personaService;

	@Autowired
	private CorreoRepositorySP correoRepositorySP;

	@Autowired
	private PersonaApiClient personaApiClient;

	@Autowired
	private SeguridadApiClient seguridadApiClient;

	@Autowired
	private UsuarioRepositorySP usuarioRepositorySP;

	@Autowired
	private ArchivoRepository archivoRepository;

	@Autowired
	private EntidadRepository entidadRepository;

	@Autowired
	private CuentaEntidadRepository cuentaEntidadRepository;

	@Autowired
	private ValidacionEntidadRepository validacionEntidadRepository;

	@Autowired
	private SolicitudRevisionRepository solicitudRevisionRepository;

	@Autowired
	private SolicitudRevisionDetalleRepository solicitudRevisionDetalleRepository;

	@Autowired
	private CargoRepository cargoRepository;

	@Autowired
	private GestionRepository gestionRepository;

	@Autowired
	private VariablesSistema variablesSistema;

	@Autowired
	private GeneralRepository generalRepositorySP;

	@Autowired
	private BeanAdapterServidorCivil beanAdapterServidorCivil;
		
	@Override
	public RespBase<RespSolicitudDuplicado> validarSolicitudDuplicado (Long ruc, Long dni) {
		Boolean response = false;
		
		List<SolicitudPersona> solicitud = new ArrayList<>();
		solicitud = solicitudPersonaRepository.findByNroRUC(ruc.toString(),variablesSistema.tipoPersonaJuridico);
		Long nroSolicitudes = 0L;
		if (!solicitud.isEmpty()) {
			List<EstadoSolicitud> solicitudes = solicitudRepositorySP.buscarSolicitudByNro(ruc.toString());
			if (!solicitudes.isEmpty()) {
				nroSolicitudes = solicitudes.stream()
				.filter(p -> p.getIdEstadoSolicitud().equals(variablesSistema.estadoSolicitudNuevo)
						|| p.getIdEstadoSolicitud().equals(variablesSistema.estadoSolicitudOserbado))
				.count();
			}
			response = nroSolicitudes == 0L ? false : true;
			// solicitud = solicitudPersonaRepository.findByEntidadIdNroDNI(solicitud.get(0).getSolicitudEntidadId(), dni.toString(),variablesSistema.tipoPersonaNatural);
			// response = solicitud.size() == 0 ? false : true;
		}
		
		RespSolicitudDuplicado respPayload = new RespSolicitudDuplicado();
		respPayload.setSolicitudExistente(response);
		return new RespBase<RespSolicitudDuplicado>().ok(respPayload);
	}
	
	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespCreaSolicitud> creaSolicitudEntidad(ReqBase<ReqCreaSolicitud> request, MyJsonWebToken token) throws IOException {
		Solicitud solicitudEntidad = new Solicitud();
		List<SolicitudPersona> solicitud = new ArrayList<>();
		List<SolicitudArchivo> listaArchivoSolicitud = new ArrayList<>();
		List<SolicitudPersona> listaPersona = new ArrayList<>();
		Boolean solicitudExistente = false;
		Long idSolicitudEntidad = null;
		String nroRuc=Constantes.VACIO;
		String nroDni=Constantes.VACIO;
		for (SolicitudPersonaDTO personaDTO : request.getPayload().getListaSolicitudPersona()) {
			if (personaDTO.getTipoPersona() == variablesSistema.tipoPersonaJuridico) { 
				nroRuc = personaDTO.getNumeroDocumento();
			}
			if (personaDTO.getTipoPersona() == variablesSistema.tipoPersonaNatural) { 
				nroDni = personaDTO.getNumeroDocumento();
			}
		}
		solicitud = solicitudPersonaRepository.findByNroRUC(nroRuc,variablesSistema.tipoPersonaJuridico);
		if (!solicitud.isEmpty()) {
			 idSolicitudEntidad = solicitud.get(0).getSolicitudEntidadId();
			 solicitud = solicitudPersonaRepository.findByEntidadIdNroDNI(idSolicitudEntidad, nroDni,variablesSistema.tipoPersonaNatural);
			 solicitudExistente = solicitud.size() == 0?false:true;
		}		 
		if (!solicitudExistente) {
		JMapper<Solicitud, SolicitudDTO> solicitudEntidadMapper = new JMapper<>(Solicitud.class, SolicitudDTO.class);
		solicitudEntidad = solicitudEntidadMapper.getDestination(request.getPayload().getSolicitud());
		solicitudEntidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
		solicitudRepository.save(solicitudEntidad);
		
		if (request.getPayload().getListaSolicitudAdjunto() != null) {
			for (SolicitudArchivoDTO archivoDTO : request.getPayload().getListaSolicitudAdjunto()) {
				SolicitudArchivo archivo = new SolicitudArchivo();
				archivo.setSolicitudEntidadId(solicitudEntidad.getSolicitudEntidadId());
				archivo.setNombreArchivo(archivoDTO.getNombreArchivo());
				archivo.setNombreRealArchivo(archivoDTO.getNombreRealArchivo());
				archivo.setTipoArchivo(archivoDTO.getTipoArchivo());
				archivo.setArchivo(archivoDTO.getArchivo());
				archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
				solicitudArchivoRepository.save(archivo);
				listaArchivoSolicitud.add(archivo);
			}
		}
		for (SolicitudPersonaDTO personaDTO : request.getPayload().getListaSolicitudPersona()) {
			SolicitudPersona persona = new SolicitudPersona();
			persona.setSolicitudEntidadId(solicitudEntidad.getSolicitudEntidadId());
			persona.setRolEntidadId(personaDTO.getRolEntidadId());
			persona.setTipoPersona(personaDTO.getTipoPersona());
			persona.setTipoDocumento(personaDTO.getTipoDocumento());
			persona.setNumeroDocumento(personaDTO.getNumeroDocumento());
			persona.setValidar(personaDTO.getValidar());
			persona.setRazonSocial(personaDTO.getRazonSocial());
			persona.setNombreComercial(personaDTO.getNombreComercial());
			persona.setSigla(personaDTO.getSigla());							
			persona.setNombres(personaDTO.getNombres());
			persona.setApellidoPaterno(personaDTO.getApellidoPaterno());
			persona.setApellidoMaterno(personaDTO.getApellidoMaterno());
			persona.setApellidoCasada(personaDTO.getApellidoCasada());
			persona.setFechaNacimiento(personaDTO.getFechaNacimiento());
			persona.setSexo(personaDTO.getSexo());
			persona.setEstadoCivil(personaDTO.getEstadoCivil());
			persona.setImagen(personaDTO.getImagen());
			persona.setCargoId(personaDTO.getCargoId());
			persona.setPuestoTrabajoId(personaDTO.getPuestoTrabajoId());
			persona.setFechaInscripcion(personaDTO.getFechaInscripcion());
			persona.setFechaInicioActividades(personaDTO.getFechaInicioActividades());
			persona.setActividadEconomicaPrincipal(personaDTO.getActividadEconomicaPrincipal());
			persona.setCondicionContribuyente(personaDTO.getCondicionContribuyente());
			persona.setEstadoConstribuyente(personaDTO.getEstadoConstribuyente());
			if (personaDTO.getTipoDocumento().equals(variablesSistema.tipoDocumentoDni)) {
				personaDTO.setPaisId(variablesSistema.idPaisPeru);
			}
			persona.setPaisId(personaDTO.getPaisId());
			persona.setUbigeoId(personaDTO.getUbigeoId());
			persona.setDireccionCompleta(personaDTO.getDireccionCompleta());
			persona.setReferenciaDireccion(personaDTO.getReferenciaDireccion());
			persona.setCorreoPrincipal(personaDTO.getCorreoPrincipal());
			persona.setCorreoSecundario(personaDTO.getCorreoSecundario());
			persona.setCorreoLaboral(personaDTO.getCorreoLaboral());
			persona.setTelefonoFijo(personaDTO.getTelefonoFijo());
			persona.setCelularPrincipal(personaDTO.getCelularPrincipal());
			persona.setCelularSecundario(personaDTO.getCelularSecundario());
			persona.setCelularLaboral(personaDTO.getCelularLaboral());
			persona.setRutaPaginaWeb(personaDTO.getRutaPaginaWeb());
			persona.setLugarNacimiento(personaDTO.getLugarNacimiento());
			solicitudPersonaRepository.save(persona);
			if (personaDTO.getListaArchivo() != null && Boolean.FALSE.equals(personaDTO.getListaArchivo().isEmpty())) {
				List<SolicitudArchivo> listaArchivosPersona = new ArrayList<>();
				for (SolicitudArchivoDTO adjuntoDTO : personaDTO.getListaArchivo()) {
					SolicitudArchivo archivo = new SolicitudArchivo();
					archivo.setSolicitudEntidadId(solicitudEntidad.getSolicitudEntidadId());
					archivo.setSolicitudPersonaId(persona.getSolicitudPersonaId());
					archivo.setNombreArchivo(adjuntoDTO.getNombreArchivo());
					archivo.setNombreRealArchivo(adjuntoDTO.getNombreRealArchivo());

					//convertir el adjuntoDTO.getArchivo() a MultipartFile:
					String route = saveFileToNgnxToReturnRoute(FilesUtil.convertirBase64ToMultipartFile(adjuntoDTO.getArchivo()));
					archivo.setRutaUpload(route);
					archivo.setTipoArchivo(adjuntoDTO.getTipoArchivo());

					archivo.setArchivo(adjuntoDTO.getArchivo());
					archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
					solicitudArchivoRepository.save(archivo);
					listaArchivosPersona.add(archivo);
				}
				persona.setListaArchivo(listaArchivosPersona);
			}
			listaPersona.add(persona);
		}
		if (solicitudEntidad.getSolicitudEntidadId() != null) {
			enviarCorreoCrearSolicitud(solicitudEntidad.getSolicitudEntidadId(), Constantes.PLANTILLA_CREAR_SOLICITUD);
		}
	}
		
		RespCreaSolicitud respPayload = new RespCreaSolicitud();
		respPayload.setSolicitudEntidad(solicitudEntidad);
		respPayload.setListaSolicitudArchivo(listaArchivoSolicitud);
		respPayload.setListaSolicitudPersona(listaPersona);
		respPayload.setSolicitudExistente(solicitudExistente);
		return new RespBase<RespCreaSolicitud>().ok(respPayload);
	}

	public String buscarParametroSolicitud(String tipoParametro, Long identificador) {
		String texto = null;
		RespBase<RespObtieneLista> respuestParametro = maestraApiClient.obtieneParametros(tipoParametro);
		if (Boolean.TRUE.equals(respuestParametro.getStatus().getSuccess()) && ParametrosUtil.listaNoVacia(respuestParametro.getPayload().getItems())) {
			for (RespObtieneLista.Parametro parametro : respuestParametro.getPayload().getItems()) {
				if (parametro.getParametroId().longValue() == identificador.longValue()) {
					texto = parametro.getValorTexto();
					break;
				}
			}			
		}
		return texto;
	}

	public String buscarParametrobyCodigo(String tipoParametro, Integer codigoNumero) {
		String texto = null;
		RespBase<RespObtieneLista> respuestParametro = maestraApiClient.obtieneParametros(tipoParametro);
		if (Boolean.TRUE.equals(respuestParametro.getStatus().getSuccess()) && ParametrosUtil.listaNoVacia(respuestParametro.getPayload().getItems())) {
			for (RespObtieneLista.Parametro parametro : respuestParametro.getPayload().getItems()) {
				if (codigoNumero.equals(parametro.getCodigoNumero())) {
					texto = parametro.getValorTexto();
					break;
				}
			}
		}
		return texto;
	}

	public String buscarPais(Long identificador) {
		String texto = null;
		RespBase<RespObtenerPais> respuestPais = maestraApiClient.buscarPais(identificador, null);
		if (Boolean.TRUE.equals(respuestPais.getStatus().getSuccess()) && ParametrosUtil.listaNoVacia(respuestPais.getPayload().getItems())) {
			texto = respuestPais.getPayload().getItems().get(0).getNombrePais();
		}
		return texto;
	}

	public String buscarCargo(Long identificador) {
		String texto = null;
		if (identificador != null) {
			Optional<Cargo> cargo = cargoRepository.findById(identificador);
			if (cargo.isPresent()) {
				texto = cargo.get().getDescripcion();
			}
		}
		return texto;
	}

	@Override
	public RespBase<RespObtieneSolicitudEntidadById> obtieneSolicitudEntidadById(Long solicitudEntidadId) {//NOSONAR
		Optional<Solicitud> solicitud = solicitudRepository.findById(solicitudEntidadId);
		RespObtieneSolicitudEntidadById respPayload = new RespObtieneSolicitudEntidadById();
		if (solicitud.isPresent()) {
			Solicitud solicitudBean = solicitud.get();
			if (solicitudBean.getNivelGobierno() != null) {
				solicitudBean.setDescripcionNivelGobierno(buscarParametroSolicitud(Constantes.PARAMETRO_NIVEL_GOBIERNO,
						solicitudBean.getNivelGobierno()));
			}
			if (solicitudBean.getSector() != null) {
				solicitudBean.setDescripcionSector(buscarParametroSolicitud(Constantes.PARAMETRO_SECTOR, solicitudBean.getSector()));
			}
			respPayload.setSolicitudEntidad(solicitudBean);
			List<SolicitudPersona> listaPersonasJuridica = buscarPersonas(solicitudEntidadId,variablesSistema.tipoPersonaJuridico, null, true);
			respPayload.setListaSolicitudPersona(listaPersonasJuridica);
			List<SolicitudPersona> listaPersonasNaturales = buscarPersonas(solicitudEntidadId,variablesSistema.tipoPersonaNatural, null, true);
			if (listaPersonasNaturales != null) {
				for (SolicitudPersona personaNatural : listaPersonasNaturales) {
					if (personaNatural.getCargoId() != null) {
						personaNatural.setDescripcionCargo(buscarCargo(personaNatural.getCargoId()));
					}
					if (personaNatural.getPaisId() != null) {
						personaNatural.setDescripcionPais(buscarPais(personaNatural.getPaisId()));
					}
					if (personaNatural.getTipoDocumento() != null) {
						personaNatural.setDescripcionTipoDoc(buscarParametrobyCodigo(Constantes.PARAMETRO_TIPO_DOC,personaNatural.getTipoDocumento()));
					}
				}
			}
			respPayload.getListaSolicitudPersona().addAll(listaPersonasNaturales);
			List<SolicitudArchivo> listaArchivo = buscarArchivosSolicitud(solicitudEntidadId);
			respPayload.setListaSolicitudArchivo(listaArchivo);
		}
		return new RespBase<RespObtieneSolicitudEntidadById>().ok(respPayload);
	}

	public List<SolicitudArchivo> buscarArchivosSolicitud(Long solicitudEntidadId) {
		SolicitudArchivo archivo = new SolicitudArchivo();
		archivo.setSolicitudEntidadId(solicitudEntidadId);
		archivo.setSolicitudPersonaId(null);
		archivo.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
		ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll()
				.withIgnorePaths("solicitudArchivoId", "nombreArchivo", "nombreRealArchivo", "tipoArchivo", "archivo",
						"usuarioCreacion", "fechaCreacion", "usuarioModificacion", "fechaModificacion", "nombreArchivo")
				.withIncludeNullValues();
		Example<SolicitudArchivo> exampleArchivo = Example.of(archivo, exampleMatcher);
		return solicitudArchivoRepository.findAll(exampleArchivo) ;
	}
	
	@Override
	public RespBase<RespObtieneSolicitudEntidad> obtieneSolicitudEntidad() {
		List<Solicitud> listaSolicitud = solicitudRepository.findAll();
		RespObtieneSolicitudEntidad respPayload = new RespObtieneSolicitudEntidad();
		respPayload.setListaSolicitud(listaSolicitud);
		return new RespBase<RespObtieneSolicitudEntidad>().ok(respPayload);

	}

	@Override
	public RespBase<RespObtieneSolicitud> buscarSolicituByFilter(Map<String, Object> parametroMap) {
		List<ObtenerSolicitud> lista = solicitudRepositorySP.buscarSolicitudFilter(parametroMap);
		RespObtieneSolicitud respPayload = new RespObtieneSolicitud();
		respPayload.setListaObtenerSolicitud(lista);
		return new RespBase<RespObtieneSolicitud>().ok(respPayload);
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespActualizaSolicitud> actualizarSolicitud(ReqBase<ReqActualizaSolicitud> request,//NOSONAR
			MyJsonWebToken token, Long solicitudId) {
		RespActualizaSolicitud respPayload = new RespActualizaSolicitud();
		Optional<Solicitud> solicitud = solicitudRepository.findById(solicitudId);
		if (solicitud.isPresent()) {
			Solicitud solicitudFinal = solicitud.get();
			solicitudFinal.setTipo(request.getPayload().getSolicitud().getTipo());
			solicitudFinal.setAceptoTerminosCondiciones(request.getPayload().getSolicitud().getAceptoTerminosCondiciones());
			solicitudFinal.setNivelGobierno(request.getPayload().getSolicitud().getNivelGobierno());
			solicitudFinal.setSector(request.getPayload().getSolicitud().getSector());
			solicitudFinal.setEstadoSolicitud(variablesSistema.estadoSolicitudNuevo);
			solicitudFinal.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),Instant.now());
			solicitudRepository.save(solicitudFinal);
			SolicitudRevision solicitudRevision = new SolicitudRevision();
			solicitudRevision.setSolicitudEntidadId(solicitudFinal.getSolicitudEntidadId());
			solicitudRevision.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
			solicitudRevision.setEstadoRevision(variablesSistema.estadoSolicitudOserbado);
			Example<SolicitudRevision> example = Example.of(solicitudRevision);
			List<SolicitudRevision> ltaRevision = solicitudRevisionRepository.findAll(example);
			if (ltaRevision != null && Boolean.FALSE.equals(ltaRevision.isEmpty())) {
				for (SolicitudRevision revision : ltaRevision) {
					if (revision.getCodigo() != null && !Strings.isEmpty(revision.getCodigo())) {
						revision.setCodigo(null);
						revision.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),Instant.now());
						solicitudRevisionRepository.save(revision);
						break;
					}
				}
			}
			List<SolicitudArchivo> listaArchivoSolicitud = new ArrayList<>();
			if (request.getPayload().getListaSolicitudAdjunto() != null) {
				for (SolicitudArchivoDTO archivoDTO : request.getPayload().getListaSolicitudAdjunto()) {
					SolicitudArchivo archivo = null;
					if (archivoDTO.getSolicitudArchivoId() != null) {
						Optional<SolicitudArchivo> solicitudArchivo = solicitudArchivoRepository
								.findById(archivoDTO.getSolicitudArchivoId());
						if (solicitudArchivo.isPresent()) {
							archivo = solicitudArchivo.get();
							archivo.setCampoSegUpd((Strings.isEmpty(archivoDTO.getEstado()) ? EstadoRegistro.ACTIVO.getCodigo(): archivoDTO.getEstado()),token.getUsuario().getUsuario(), Instant.now());
						} else {
							archivo = new SolicitudArchivo();
							archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
						}
					} else {
						archivo = new SolicitudArchivo();
						archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
					}
					archivo.setSolicitudEntidadId(solicitudFinal.getSolicitudEntidadId());
					archivo.setSolicitudPersonaId(null);
					archivo.setNombreArchivo(archivoDTO.getNombreArchivo());
					archivo.setNombreRealArchivo(archivoDTO.getNombreRealArchivo());
					archivo.setTipoArchivo(archivoDTO.getTipoArchivo());
					archivo.setArchivo(archivoDTO.getArchivo());
					solicitudArchivoRepository.save(archivo);
					listaArchivoSolicitud.add(archivo);
				}
			}
			respPayload.setListaSolicitudArchivo(listaArchivoSolicitud);
			List<SolicitudPersona> listaPersona = new ArrayList<>();
			for (SolicitudPersonaDTO personaDTO : request.getPayload().getListaSolicitudPersona()) {
				if (personaDTO.getSolicitudPersonaId() != null) {
					Optional<SolicitudPersona> personaBD = solicitudPersonaRepository.findById(personaDTO.getSolicitudPersonaId());
					if (personaBD.isPresent()) {
						SolicitudPersona persona = personaBD.get();
						persona.setRolEntidadId(personaDTO.getRolEntidadId());
						persona.setTipoPersona(personaDTO.getTipoPersona());
						persona.setTipoDocumento(personaDTO.getTipoDocumento());
						persona.setNumeroDocumento(personaDTO.getNumeroDocumento());
						persona.setValidar(personaDTO.getValidar());
						persona.setRazonSocial(personaDTO.getRazonSocial());
						persona.setNombreComercial(personaDTO.getNombreComercial());
						persona.setSigla(personaDTO.getSigla());
						persona.setNombres(personaDTO.getNombres());
						persona.setApellidoPaterno(personaDTO.getApellidoPaterno());
						persona.setApellidoMaterno(personaDTO.getApellidoMaterno());
						persona.setApellidoCasada(personaDTO.getApellidoCasada());
						persona.setFechaNacimiento(personaDTO.getFechaNacimiento());
						persona.setSexo(personaDTO.getSexo());
						persona.setEstadoCivil(personaDTO.getEstadoCivil());
						persona.setImagen(personaDTO.getImagen());
						persona.setCargoId(personaDTO.getCargoId());
						persona.setPuestoTrabajoId(personaDTO.getPuestoTrabajoId());
						persona.setFechaInscripcion(personaDTO.getFechaInscripcion());
						persona.setFechaInicioActividades(personaDTO.getFechaInicioActividades());
						persona.setActividadEconomicaPrincipal(personaDTO.getActividadEconomicaPrincipal());
						persona.setCondicionContribuyente(personaDTO.getCondicionContribuyente());
						persona.setEstadoConstribuyente(personaDTO.getEstadoConstribuyente());
						if (personaDTO.getTipoDocumento().intValue() == variablesSistema.tipoDocumentoDni) {
							personaDTO.setPaisId(variablesSistema.idPaisPeru);
						}
						persona.setPaisId(personaDTO.getPaisId());
						persona.setUbigeoId(personaDTO.getUbigeoId());
						persona.setDireccionCompleta(personaDTO.getDireccionCompleta());
						persona.setReferenciaDireccion(personaDTO.getReferenciaDireccion());
						persona.setCorreoPrincipal(personaDTO.getCorreoPrincipal());
						persona.setCorreoSecundario(personaDTO.getCorreoSecundario());
						persona.setCorreoLaboral(personaDTO.getCorreoLaboral());
						persona.setTelefonoFijo(personaDTO.getTelefonoFijo());
						persona.setCelularPrincipal(personaDTO.getCelularPrincipal());
						persona.setCelularSecundario(personaDTO.getCelularSecundario());
						persona.setCelularLaboral(personaDTO.getCelularLaboral());
						persona.setRutaPaginaWeb(personaDTO.getRutaPaginaWeb());
						persona.setLugarNacimiento(personaDTO.getLugarNacimiento());
						solicitudPersonaRepository.save(persona);
						if (personaDTO.getListaArchivo() != null && Boolean.FALSE.equals(personaDTO.getListaArchivo().isEmpty())) {
							List<SolicitudArchivo> listaArchivosPersona = new ArrayList<>();
							for (SolicitudArchivoDTO adjuntoDTO : personaDTO.getListaArchivo()) {
								SolicitudArchivo archivo = null;
								if (adjuntoDTO.getSolicitudArchivoId() != null) {
									Optional<SolicitudArchivo> solicitudArchivo = solicitudArchivoRepository
											.findById(adjuntoDTO.getSolicitudArchivoId());
									if (solicitudArchivo.isPresent()) {
										archivo = solicitudArchivo.get();
										archivo.setCampoSegUpd((Strings.isEmpty(adjuntoDTO.getEstado())? EstadoRegistro.ACTIVO.getCodigo(): adjuntoDTO.getEstado()),token.getUsuario().getUsuario(), Instant.now());
									} else {
										archivo = new SolicitudArchivo();
										archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
									}
								} else {
									archivo = new SolicitudArchivo();
									archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
								}
								archivo.setSolicitudEntidadId(persona.getSolicitudEntidadId());
								archivo.setSolicitudPersonaId(persona.getSolicitudPersonaId());
								archivo.setNombreArchivo(adjuntoDTO.getNombreArchivo());
								archivo.setNombreRealArchivo(adjuntoDTO.getNombreRealArchivo());
								archivo.setTipoArchivo(adjuntoDTO.getTipoArchivo());
								archivo.setArchivo(adjuntoDTO.getArchivo());
								solicitudArchivoRepository.save(archivo);
								listaArchivosPersona.add(archivo);
							}
							persona.setListaArchivo(listaArchivosPersona);
						}
						listaPersona.add(persona);
					}
				}
			}
			respPayload.setSolicitudEntidad(solicitudFinal);
			respPayload.setListaSolicitudPersona(listaPersona);
			return new RespBase<RespActualizaSolicitud>().ok(respPayload);
		} else {
			RespBase<RespActualizaSolicitud> error = new RespBase<>();
			error.getStatus().setSuccess(Boolean.FALSE);
			error.getStatus().getError().getMessages().add("No existe la solicitudId Ingresada");
			return error;
		}
	}

	public void enviarCorreoCrearSolicitud(Long solicitudEntidadId, String plantilla) {

		Map<String, Object> parametrosCorreo = new HashMap<>();
		List<String> correosEnvio = new ArrayList<>();
		SolicitudPersona solicitudPersona = new SolicitudPersona();
		solicitudPersona.setSolicitudEntidadId(solicitudEntidadId);
		Example<SolicitudPersona> example = Example.of(solicitudPersona);
		List<SolicitudPersona> listaPersonas = solicitudPersonaRepository.findAll(example);
		for (SolicitudPersona persona : listaPersonas) {
			if (!Strings.isEmpty(persona.getCorreoPrincipal()) && persona.getCorreoPrincipal() != null) {
				correosEnvio.add(persona.getCorreoPrincipal());
			}
		}
		if (Boolean.FALSE.equals(correosEnvio.isEmpty())) {
			parametrosCorreo.put(Constantes.CORREOS_ENVIO, correosEnvio);
			Map<String, Object> parametrosPlantilla = new HashMap<>();
			parametrosCorreo.put(Constantes.PARAMETROS, parametrosPlantilla);
			notificacionService.enviarNotificacion(Constantes.ASUNTO_CREAR_SOLICITUD, plantilla, parametrosCorreo,
					true);
		}
	}

	public void enviarCorreoConfirmacion(String correoEnvio, String plantilla,String codigoConfirmacion) {			
			notificacionService.enviarCodigoConfirmacion(Constantes.ASUNTO_CONFIRMACION_DE_CORREO, plantilla, correoEnvio,
					true, codigoConfirmacion);
	}
	
	@SuppressWarnings("rawtypes")
	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> darAltaSolicitud(MyJsonWebToken token, Long solicitudId) {//NOSONAR
		RespBase<Object> response = new RespBase<>();
		Entidad entidad = null;
		try {
			Optional<Solicitud> solicitud = solicitudRepository.findById(solicitudId);
			System.out.println("solicitud = " + solicitud);
			if (solicitud.isPresent()) {
				Solicitud solicitudResponse = solicitud.get();
				if (solicitudResponse.getEstadoSolicitud().intValue() == variablesSistema.estadoSolicitudAprobado) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"LA SOLICITUD YA SE ENCUENTRA ACTUALMENTE APROBADA");
					response.setPayload(solicitud);
					return response;
				}
				List<SolicitudPersona> listaPersonaNatural = buscarPersonas(solicitudResponse.getSolicitudEntidadId(),
						variablesSistema.tipoPersonaNatural, null, true);
				if (!listaPersonaNatural.isEmpty()) {
					List<EntidadDTO> ltaEntidad = gestionRepository.buscarEntidadByPersona(listaPersonaNatural.get(0).getTipoDocumento(),listaPersonaNatural.get(0).getNumeroDocumento());
					if (!ltaEntidad.isEmpty()) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"No se puede registrar al administrador : La persona  ya forma parte de otra entidad "	+ ltaEntidad.get(0).getDescripcionEntidad());
						response.setPayload(solicitud);
						return response;
					}
				}
				Long personaJuridicaId = null;
				List<SolicitudPersona> listaPersonaJuridica = buscarPersonas(solicitudResponse.getSolicitudEntidadId(),
						variablesSistema.tipoPersonaJuridico, null, true);
				if (!listaPersonaJuridica.isEmpty()) {
					RespBase<ApiPersonaRequestDTO> personaJuridicaResquest = new RespBase<>();
					RespBase<RespApiPersona> responsePer = personaApiClient.obtienePersonaPorDocumento(listaPersonaJuridica.get(0).getTipoDocumento(),
							listaPersonaJuridica.get(0).getNumeroDocumento());
					ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaJuridica> apiPersonaJuridica = new ApiPersonaRequestDTO<>();
					ParametrosUtil.setearPersonaJuridica(apiPersonaJuridica, listaPersonaJuridica.get(0),variablesSistema.tipoDocumentoRuc,responsePer);
					personaJuridicaResquest.setPayload(apiPersonaJuridica);
					response = personaService.obtenerInsertarPersona(variablesSistema.tipoPersonaJuridico,listaPersonaJuridica.get(0).getTipoDocumento(),	listaPersonaJuridica.get(0).getNumeroDocumento(), personaJuridicaResquest);
				} else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"NO SE ENCONTRO PERSONA JURIDICA");
					response.setPayload(solicitud);
				}
				System.out.println("response.getStatus() = " + response.getStatus());
				if (Boolean.TRUE.equals(response.getStatus().getSuccess())) {
					RespApiPersona beanPersonaJuridica = (RespApiPersona) response.getPayload();
					personaJuridicaId = beanPersonaJuridica.getPersona().getPersonaId();
					Entidad entidadBusqueda = new Entidad();
					entidadBusqueda.setPersonaId(personaJuridicaId);
					entidadBusqueda.setTipoEntidadId(variablesSistema.tipoPersonaJuridico);
					entidadBusqueda.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
					Example<Entidad> exampleEntidad = Example.of(entidadBusqueda);
					List<Entidad> listaEntidad = entidadRepository.findAll(exampleEntidad);
					Parametro parametro = generalRepositorySP.buscarParametro(null, null,Constantes.RUTA_FILE_SERVER_ENTIDAD);
					String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);
					if (!listaEntidad.isEmpty()) {
						entidad = listaEntidad.get(0);
						Solicitud solicitudFind = new Solicitud();
						solicitudFind.setEntidadId(entidad.getEntidadId());
						solicitudFind.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
						Example<Solicitud> exampleSoli = Example.of(solicitudFind);
						List<Solicitud> ltaSoli = solicitudRepository.findAll(exampleSoli);
						for (Solicitud soli : ltaSoli) {
							if (soli.getEstadoSolicitud().intValue() != variablesSistema.estadoBaja&& soli.getFechaBaja() == null&& soli.getSolicitudEntidadId().longValue() != solicitudId.longValue()) {
								soli.setEstadoSolicitud(variablesSistema.estadoBaja);
								soli.setFechaBaja(ParametrosUtil.ObtenerFechaActual());
								soli.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),	Instant.now());
								solicitudRepository.save(soli);
							}
						}
						entidad.setFlagActualiza(Constantes.FLAG_NO_ACTUALIZO_ENTIDAD);
						entidad.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
						entidad.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),Instant.now());
						entidadRepository.save(entidad);
						rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,
								entidad.getEntidadId().toString());
						List<CuentaEntidadRolDTO> listaCuentaEntidadUser = usuarioRepositorySP.buscarRolCuentaEntidad(entidad.getEntidadId(), variablesSistema.rolAdminEntidad);
						for (CuentaEntidadRolDTO cuentaClienteRolDTO : listaCuentaEntidadUser) {
							boolean encontro = false;
							for (SolicitudPersona person : listaPersonaNatural) {
								if (person.getTipoDocumento().intValue() == cuentaClienteRolDTO.getTipoDocumento().intValue()	&& person.getNumeroDocumento().equals(cuentaClienteRolDTO.getNumeroDocumento())) {
									encontro = true;
									break;
								}
							}
							if (!encontro) {
								RespBase<ApiActualizarRolUsuario> requestActualizar = new RespBase<>();
								ApiActualizarRolUsuario item = new ApiActualizarRolUsuario();
								item.setUsuarioRolId(cuentaClienteRolDTO.getUsuarioRolId());
								item.setFechaInicioVigencia(cuentaClienteRolDTO.getFechaInicioVigencia());
								item.setFechaFinVigencia(ParametrosUtil.fechaHoraActualString());
								item.setEstado(Constantes.ESTADO_INACTIVACION_USUARIO);
								requestActualizar.setPayload(item);

								response = seguridadApiClient.actualizarEstadoRol(cuentaClienteRolDTO.getUsuarioRolId(),requestActualizar);

								if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
									return response;
								}
							}
						}
						List<CuentaEntidadDTO> listaCuentasEntidad = generalRepositorySP.buscarCuentaEntidad(entidad.getEntidadId());
						for (CuentaEntidadDTO cuentaEntidadDTO : listaCuentasEntidad) {
							boolean encontro = false;
							for (SolicitudPersona person : listaPersonaNatural) {
								if (person.getTipoDocumento().intValue() == cuentaEntidadDTO.getTipoDocumento().intValue() && person.getNumeroDocumento().equals(cuentaEntidadDTO.getNumeroDocumento())) {
									person.setCuentaClienteId(cuentaEntidadDTO.getCuentaEntidadId());
									encontro = true;
									break;
								}
							}
							if (!encontro) {
								Optional<CuentaEntidad> cuentaEntidad = cuentaEntidadRepository.findById(cuentaEntidadDTO.getCuentaEntidadId());
								if (cuentaEntidad.isPresent()) {
									CuentaEntidad cuenta = cuentaEntidad.get();
									cuenta.setCampoSegUpd(EstadoRegistro.INACTIVO.getCodigo(),token.getUsuario().getUsuario(), Instant.now());
									cuentaEntidadRepository.save(cuenta);
								}
							}
						}
					} else {
						entidad = new Entidad();
						entidad.setPersonaId(personaJuridicaId);
						entidad.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
						entidad.setUbigeoId((beanPersonaJuridica.getDirecciones() != null && !beanPersonaJuridica.getDirecciones().isEmpty())	? beanPersonaJuridica.getDirecciones().get(0).getUbigeoId()	: null);
						entidad.setNivelGobiernoId(solicitudResponse.getNivelGobierno());
						entidad.setSectorId(solicitudResponse.getSector());
						entidad.setTipoEntidadId(variablesSistema.tipoPersonaJuridico);
						entidad.setFlagActualiza(Constantes.FLAG_NO_ACTUALIZO_ENTIDAD);
						entidad.setDescripcionEntidad(beanPersonaJuridica.getPersonaJuridica().getRazonSocial());
						entidad.setUrlWebEntidad((beanPersonaJuridica.getWebs() != null && !beanPersonaJuridica.getWebs().isEmpty())? beanPersonaJuridica.getWebs().get(0).getUrlWeb(): null);
						entidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
						entidadRepository.save(entidad);
						rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,entidad.getEntidadId().toString());
					}
					for (SolicitudPersona person : listaPersonaNatural) {
						RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
						ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();
						ParametrosUtil.setearPersonaNatural(apiPersonaNatural, getPersonaDTO(person));
						List<ApiPersonaRequestDTO.Correo> correosPersona = new ArrayList<>(apiPersonaNatural.getCorreos());
						apiPersonaNatural.getCorreos().clear();
						personaNaturalResquest.setPayload(apiPersonaNatural);
						response = personaService.obtenerInsertarPersona(variablesSistema.tipoPersonaNatural,person.getTipoDocumento(), person.getNumeroDocumento(), personaNaturalResquest);
						if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
							return response;
						}
						RespApiPersona personaResponse = (RespApiPersona) response.getPayload();
						person.setPersonaId(personaResponse.getPersona().getPersonaId());
						person.setTelefonoId(personaResponse.getPersona().getTelefonoId());
						List<RespApiPersona.Correo> correosResponse = new ArrayList<>();
						for (ApiPersonaRequestDTO.Correo correoPersona : correosPersona) {
							List<CorreoApiDTO> correoRpta = correoRepositorySP.buscarEmailPersona(correoPersona.getCorreo());
							if (correoRpta == null || Boolean.TRUE.equals(correoRpta.isEmpty())) {
								RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<>();
								ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(null,correoPersona.getTipoCorreo(), correoPersona.getCorreo());
								requestCorreo.setPayload(correoPayload);
								RespBase<RespApiPersona.Correo> respuesta = personaApiClient.crearCorreo(personaResponse.getPersona().getPersonaId().intValue(), requestCorreo);
								if (Boolean.TRUE.equals(respuesta.getStatus().getSuccess())) {
									person.setCorreoId(respuesta.getPayload().getCorreoId());
									correosResponse.add(respuesta.getPayload());
								} else {
									response = ParametrosUtil.setearResponse(response, Boolean.FALSE,respuesta.getStatus().getError().toString());
									return response;
								}
							} else {
								
								correosResponse.add(beanAdapterServidorCivil.adapterCorreo(correoRpta));
								person.setCorreoId(correoRpta.get(0).getCorreoId());
							}
						}
						personaResponse.setCorreos(correosResponse);
						String nroDocumento = personaResponse.getDocumentos().get(0).getNumeroDocumento();

						if (person.getRolEntidadId() != null && person.getRolEntidadId()
								.longValue() == variablesSistema.rolAdminEntidad.longValue()) {
							RespBase<RespApiObtenerUsuario> rptaUsuario = seguridadApiClient.buscarUsuariosByFiltro(nroDocumento, null, null, Constantes.ESTADO_ACTIVO);
							System.out.println("rptaUsuario = " + rptaUsuario);
							if (Boolean.FALSE.equals(rptaUsuario.getStatus().getSuccess())) {
								response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
										rptaUsuario.getStatus().getError().toString());
								return response;
							}
							Long usuarioId = null;
							if (rptaUsuario.getPayload().getCount() == 0 || rptaUsuario.getPayload().getItems() == null	|| rptaUsuario.getPayload().getItems().isEmpty()) {
								RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
								ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(nroDocumento,personaResponse.getCorreos().get(0).getCorreo(),personaResponse.getPersona().getPersonaId(),
										Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, null);
								apiUsuario.setPayload(requestCreaUsuario);
								System.out.println("apiUsuario = " + apiUsuario);

								RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(entidad.getEntidadId(), apiUsuario);
								System.out.println("respuestaCreaUser = " + respuestaCreaUser);

								if (Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {

									response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
											respuestaCreaUser.getStatus().getError().toString());
									return response;
								} else {
									usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
								}
							} else {
								usuarioId = rptaUsuario.getPayload().getItems().get(0).getUsuarioId();
								
								// COMPROBAMOS QUE pertenesca a esa entidad desde el api seguridad:
								RespBase<RespApiObtenerUsuario> respuestaWS = seguridadApiClient.buscarUsuarioEntidad(entidad.getEntidadId());

								System.out.println("respuestaWS = " + respuestaWS);

								if (Boolean.FALSE.equals(respuestaWS.getStatus().getSuccess())) {
									response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
											respuestaWS.getStatus().getError().toString());
									return response;
								}
								boolean encontro = false;
								if (respuestaWS.getPayload().getItems() != null	&& !respuestaWS.getPayload().getItems().isEmpty()) {
									for (RespApiSeguridad itemSeg : respuestaWS.getPayload().getItems()) {
										if (itemSeg.getUsuarioId().longValue() == usuarioId.longValue()) {
											encontro = true;
											break;
										}
									}
								}
								if (!encontro) {
									response = seguridadApiClient.asignarUsuarioEntidad(entidad.getEntidadId(),usuarioId, EstadoRegistro.ACTIVO.getCodigo());
									if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
										return response;
									}
								}
								
							}
							person.setUsuarioId(usuarioId);
							enviarCorreoExisteUsuario(person.getCorreoPrincipal(),(rptaUsuario.getPayload().getItems().isEmpty()?person.getCorreoPrincipal():rptaUsuario.getPayload().getItems().get(0).getCorreoElectronico()),(rptaUsuario.getPayload().getItems().isEmpty()?person.getNumeroDocumento():rptaUsuario.getPayload().getItems().get(0).getUsuario()), false);
							if (!usuarioRepositorySP.existeRolUsuario(usuarioId,variablesSistema.rolAdminEntidad)) {
								RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
								AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
								asignaRolDTO.setRolId(variablesSistema.rolAdminEntidad);
								asignaRolDTO.setUsuarioId(usuarioId);
								requestAsignaRol.setPayload(asignaRolDTO);
								response = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
								if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
									return response;
								}
							}
							response = seguridadApiClient.asignarGrupoEntidad(entidad.getEntidadId(),Long.valueOf(variablesSistema.grupoEntidadAdmin), usuarioId);
							if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
								return response;
							}
						}


						if (person.getListaArchivo() != null && !person.getListaArchivo().isEmpty()) {
							for (SolicitudArchivo solicitudArchivo : person.getListaArchivo()) {
								RespBase<ApiUploadFile> requestApiUploadFile = new RespBase<>();
								ApiUploadFile uploadFile = new ApiUploadFile();
								uploadFile.setExtension("." + ParametrosUtil.extension(solicitudArchivo.getNombreArchivo()));
								uploadFile.setFileBase64(solicitudArchivo.getArchivo());
								uploadFile.setFileName(ParametrosUtil.onlyName(solicitudArchivo.getNombreArchivo()));
								uploadFile.setPath(rutaFileServer);
								requestApiUploadFile.setPayload(uploadFile);
								RespBase<RespUploadFile> responseWS = maestraApiClient.uploadFile(requestApiUploadFile);
								if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
									response = ParametrosUtil.setearResponse(response, Boolean.FALSE,responseWS.getStatus().getError().toString());
									return response;
								}
								solicitudArchivo.setRutaUpload(responseWS.getPayload().getPathRelative());
							}
						}
					}
					for (SolicitudPersona person : listaPersonaNatural) {
						CuentaEntidad cuentaEntidadFind = new CuentaEntidad();
						cuentaEntidadFind.setEntidadId(entidad.getEntidadId());
						cuentaEntidadFind.setPersonaId(person.getPersonaId());
						cuentaEntidadFind.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
						Example<CuentaEntidad> exampleCuentaEntidad = Example.of(cuentaEntidadFind);
						List<CuentaEntidad> listaCuentaEntidad = cuentaEntidadRepository.findAll(exampleCuentaEntidad);
						CuentaEntidad cuentaEntidad;
						if (listaCuentaEntidad != null && !listaCuentaEntidad.isEmpty()) {
							cuentaEntidad = listaCuentaEntidad.get(0);
							cuentaEntidad.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(),
									token.getUsuario().getUsuario(), Instant.now());
						} else {
							cuentaEntidad = new CuentaEntidad();
							cuentaEntidad.setEntidadId(entidad.getEntidadId());
							cuentaEntidad.setPersonaId(person.getPersonaId());
							cuentaEntidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
						}
						if (person.getRolEntidadId() != null && person.getRolEntidadId().longValue() == variablesSistema.rolAdminEntidad.longValue()) {
							cuentaEntidad.setUsuarioId(person.getUsuarioId());
						}
						cuentaEntidad.setPuestoTrabajoId(String.valueOf(person.getPuestoTrabajoId()));
						cuentaEntidad.setTelefonoId(person.getTelefonoId());
						cuentaEntidad.setCorreoId(person.getCorreoId());
						cuentaEntidadRepository.save(cuentaEntidad);
						int contador = 0;
						if (person.getListaArchivo() != null && !person.getListaArchivo().isEmpty()) {
							for (SolicitudArchivo archivoAdjunto : person.getListaArchivo()) {
								contador++;
								Archivo archivo = new Archivo();
								archivo.setRepresentanteEntidadId(cuentaEntidad.getCuentaEntidadId());
								archivo.setArchivo(archivoAdjunto.getArchivo());
								archivo.setNombreArchivo(archivoAdjunto.getNombreArchivo());
								archivo.setNombreRealAArchivo(archivoAdjunto.getNombreRealArchivo());
								archivo.setRutaArchivo(archivoAdjunto.getRutaUpload());
								archivo.setExtensionArchivo("." + ParametrosUtil.extension(archivoAdjunto.getNombreArchivo()));
								archivo.setTipoArchivo(archivoAdjunto.getTipoArchivo());
								archivo.setOrden(contador);
								archivo.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
								archivoRepository.save(archivo);
							}
						}
					}
					ValidacionEntidad validacionEntidad = new ValidacionEntidad();
					validacionEntidad.setEntidadId(entidad.getEntidadId());
					validacionEntidad.setTipoValidacion(variablesSistema.tipoValidacionSunat);
					validacionEntidad.setFlagValidado(Constantes.FLAG_ESTADO_VALIDADO);
					validacionEntidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
					validacionEntidadRepository.save(validacionEntidad);
					SolicitudRevision solicitudRevision = new SolicitudRevision();
					solicitudRevision.setSolicitudEntidadId(solicitudResponse.getSolicitudEntidadId());
					solicitudRevision.setDescripcion(Constantes.DESCRIPCION_SOLICITUD_APROBADO);
					solicitudRevision.setEnvioCorreo(Constantes.NO_ENVIO_CORREO);
					solicitudRevision.setEstadoRevision(variablesSistema.estadoSolicitudAprobado);
					solicitudRevision.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
					solicitudRevisionRepository.save(solicitudRevision);
					solicitudResponse.setFechaAlta(ParametrosUtil.ObtenerFechaActual());
					solicitudResponse.setEntidadId(entidad.getEntidadId());
					solicitudResponse.setEstadoSolicitud(variablesSistema.estadoSolicitudAprobado);
					solicitudResponse.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(),
							Instant.now());
					solicitudRepository.save(solicitudResponse);
					response.setPayload(entidad);
					response.getStatus().setSuccess(Boolean.TRUE);
				}
			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"NO SE ENCONTRO LA SOLICITUD A DAR ALTA");
				response.setPayload(solicitud);
			}
		} catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;

	}

	public List<SolicitudPersona> buscarPersonas(Long solicitudId, int tipoPersona, Long rol, boolean buscarArchivos) {
		SolicitudPersona personas = new SolicitudPersona(solicitudId, tipoPersona);
		if (rol != null) {
			personas.setRolEntidadId(rol);
		}
		Example<SolicitudPersona> examplePersonaNatural = Example.of(personas);
		List<SolicitudPersona> listaPersonaNatural = solicitudPersonaRepository.findAll(examplePersonaNatural);
		if (buscarArchivos) {
			for (SolicitudPersona solicitudPersona : listaPersonaNatural) {
				SolicitudArchivo archivo = new SolicitudArchivo();
				archivo.setSolicitudEntidadId(solicitudPersona.getSolicitudEntidadId());
				archivo.setSolicitudPersonaId(solicitudPersona.getSolicitudPersonaId());
				archivo.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
				ExampleMatcher exampleMatcher = ExampleMatcher.matchingAll().withIgnorePaths("solicitudPersonaId").withIncludeNullValues();
				Example<SolicitudArchivo> exampleArchivo = Example.of(archivo, exampleMatcher);
				List<SolicitudArchivo> listaArchivo = solicitudArchivoRepository.findAll(exampleArchivo);
				solicitudPersona.setListaArchivo(listaArchivo);
			}
		}
		return listaPersonaNatural;
	}

	public void enviarCorreoExisteUsuario(String correo, String correoUsuario, String nroDocumento,
			boolean ejecutarHilo) {
		Map<String, Object> parametrosCorreo = new HashMap<>();
		List<String> correosEnvio = new ArrayList<>();
		correosEnvio.add(correo);
		if (!correosEnvio.isEmpty()) {
			parametrosCorreo.put(Constantes.CORREOS_ENVIO, correosEnvio);
			Map<String, Object> parametrosPlantilla = new HashMap<>();
			parametrosPlantilla.put("CORREO_ELECTRONICO", correoUsuario);
			parametrosPlantilla.put("NRO_DOCUMENTO", nroDocumento);
			parametrosPlantilla.put("CORREO_SERVIR", variablesSistema.correoContacto);
			parametrosCorreo.put(Constantes.PARAMETROS, parametrosPlantilla);
			notificacionService.enviarNotificacion(Constantes.ASUNTO_CREAR_SOLICITUD,
					Constantes.PLANTILLA_USUARIO_EXISTENTE, parametrosCorreo, ejecutarHilo);
		}
	}

	public PersonaDTO getPersonaDTO(SolicitudPersona person) {

		return new PersonaDTO(person.getTipoDocumento(), person.getNumeroDocumento(),
				person.getNombres(), person.getApellidoPaterno(), person.getApellidoMaterno(),
				person.getApellidoCasada(), person.getSexo(), person.getEstadoCivil(), person.getImagen(),
				person.getCargoId(), person.getPaisId(), person.getUbigeoId(), person.getDireccionCompleta(), null,
				person.getCorreoPrincipal(), null, person.getCorreoSecundario(), person.getCorreoLaboral(), null,
				person.getTelefonoFijo(), null, null, person.getCelularPrincipal(), person.getCelularSecundario(),
				person.getCelularLaboral(), person.getRutaPaginaWeb());
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> registrarObservacion(ReqBase<ReqInsObservacion> request, MyJsonWebToken token,
			Long solicitudId) {
		RespBase<Object> response = new RespBase<>();
		Solicitud solicitudFind = new Solicitud();
		solicitudFind.setSolicitudEntidadId(solicitudId);
		solicitudFind.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
		Example<Solicitud> exampleSolicitud = Example.of(solicitudFind);
		List<Solicitud> listaSolicitud = solicitudRepository.findAll(exampleSolicitud);
		if (listaSolicitud.isEmpty()) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "No existe la solicitud Ingresada");
		} else if (listaSolicitud.get(0).getEstadoSolicitud().equals(variablesSistema.estadoSolicitudOserbado)) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"La solicitud ya fue observada, esta no puede ser observada nuevamente");
		} else if (listaSolicitud.get(0).getEstadoSolicitud().equals(variablesSistema.estadoSolicitudAprobado)) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"La solicitud ya fue aprobada, esta no puede ser observada");
		} else {
			Solicitud solicitud = listaSolicitud.get(0);
			SolicitudRevision solicitudRevision = new SolicitudRevision();
			solicitudRevision.setSolicitudEntidadId(solicitud.getSolicitudEntidadId());
			solicitudRevision.setEstadoRevision(variablesSistema.estadoSolicitudOserbado);
			solicitudRevision.setDescripcion(Constantes.DESCRIPCION_SOLICITUD_OBSERVADO);
			solicitudRevision.setEnvioCorreo(Constantes.ENVIO_CORREO);
			solicitudRevision.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			String codigoObservacion = solicitudFind.getSolicitudEntidadId().toString()+ ParametrosUtil.GeneradorCodigo();
			solicitudRevision.setCodigo(codigoObservacion);
			solicitudRevisionRepository.save(solicitudRevision);
			for (Long observacionId : request.getPayload().getListaIdObservacion()) {
				SolicitudRevisionDetalle detalle = new SolicitudRevisionDetalle();
				detalle.setSolicitudRevisionId(solicitudRevision.getSolicitudRevisionId());
				detalle.setObservacionId(observacionId);
				detalle.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
				solicitudRevisionDetalleRepository.save(detalle);
			}
			String urlEnvio = variablesSistema.urlSistema + variablesSistema.prefijoRedirecActSol + "/"	+ codigoObservacion + "/" + solicitud.getSolicitudEntidadId();
			enviarCorreoObservacion(solicitud.getSolicitudEntidadId(), urlEnvio,request.getPayload().getListaIdObservacion(), false);
			solicitud.setEstadoSolicitud(variablesSistema.estadoSolicitudOserbado);
			solicitud.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), token.getUsuario().getUsuario(), Instant.now());
			solicitudRepository.save(solicitud);
			response.getStatus().setSuccess(Boolean.TRUE);
		}
		return response;
	}

	public void enviarCorreoObservacion(Long solicitudId, String urlGenerada, List<Long> ltaObs, boolean ejecutarHilo) {//NOSONAR
		List<SolicitudPersona> personaJuridica = buscarPersonas(solicitudId, variablesSistema.tipoPersonaJuridico, null,false);
		if (personaJuridica != null && !personaJuridica.isEmpty()) {
			String nombreEntidad = personaJuridica.get(0).getRazonSocial();
			List<SolicitudPersona> personaAdministrador = buscarPersonas(solicitudId, variablesSistema.tipoPersonaNatural,variablesSistema.rolAdminEntidad, false);
			if (personaAdministrador != null && !personaAdministrador.isEmpty()) {
				Map<String, Object> parametrosCorreo = new HashMap<>();
				List<String> correosEnvio = new ArrayList<>();
				for (SolicitudPersona personaAdmin : personaAdministrador) {
					if (!Strings.isEmpty(personaAdmin.getCorreoPrincipal())&& personaAdmin.getCorreoPrincipal() != null) {
						correosEnvio.add(personaAdmin.getCorreoPrincipal());
					}
				}
				if (correosEnvio != null && !correosEnvio.isEmpty()) {
					RespBase<RespObtieneLista> listaParametros = maestraApiClient.obtieneParametros(Constantes.CODIGO_PARAMETRO_OBSERVACION);
					if (listaParametros.getPayload().getItems() != null
							&& Boolean.FALSE.equals(listaParametros.getPayload().getItems().isEmpty())) {
						String textoObservacion = "";
						for (RespObtieneLista.Parametro observacion : listaParametros.getPayload().getItems()) {
							for (Long idObservacion : ltaObs) {
								if (observacion.getParametroId().longValue() == idObservacion.longValue()) {
									textoObservacion = textoObservacion + "<br>- " + observacion.getValorTexto() + ".";
									break;
								}
							}
						}
						parametrosCorreo.put(Constantes.CORREOS_ENVIO, correosEnvio);
						Map<String, Object> parametrosPlantilla = new HashMap<>();
						parametrosPlantilla.put("NOMBRE_ENTIDAD", nombreEntidad);
						parametrosPlantilla.put("OBSERVACION", textoObservacion);
						parametrosPlantilla.put("URL_SISTEMA", urlGenerada);
						parametrosPlantilla.put("CORREO_SERVIR", variablesSistema.correoContacto);
						parametrosCorreo.put(Constantes.PARAMETROS, parametrosPlantilla);
						notificacionService.enviarNotificacion(Constantes.ASUNTO_CREAR_SOLICITUD,Constantes.CODIGO_PLANTILLA_SOLICITUD_OBSV, parametrosCorreo, ejecutarHilo);
					}
				}
			}
		}
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespObtieneSolicitudEntidadById> validarcodActualizacionSolicitud(String codigo, Long solicitudId) {
		RespBase<RespObtieneSolicitudEntidadById> response = new RespBase<>();
		RespObtieneSolicitudEntidadById obtener = new RespObtieneSolicitudEntidadById();
		SolicitudRevision solicitudRevision = new SolicitudRevision();
		solicitudRevision.setCodigo(codigo);
//		solicitudRevision.setEstadoRevision(variablesSistema.estadoSolicitudAprobado);
		solicitudRevision.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
		Example<SolicitudRevision> example = Example.of(solicitudRevision);
		List<SolicitudRevision> ltaRevision = solicitudRevisionRepository.findAll(example);
		if (ltaRevision.isEmpty()) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"No existe el codigo actualizacion ingresado.");
		} else if (!ltaRevision.isEmpty()
				&& ltaRevision.get(0).getSolicitudEntidadId().longValue() != solicitudId.longValue()) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"la solicitudId no pertenece al codigo Actualización ingresado.");
		} else {
			Optional<Solicitud> solicitud = solicitudRepository.findById(solicitudId);
			if (solicitud.isPresent()) {
				Solicitud soli = solicitud.get();
				obtener.setSolicitudEntidad(soli);
				List<SolicitudPersona> listaPersonasJuridica = buscarPersonas(solicitudId,variablesSistema.tipoPersonaJuridico, null, true);
				obtener.setListaSolicitudPersona(listaPersonasJuridica);
				List<SolicitudPersona> listaPersonasNaturales = buscarPersonas(solicitudId,variablesSistema.tipoPersonaNatural, null, true);
				obtener.getListaSolicitudPersona().addAll(listaPersonasNaturales);
				List<SolicitudArchivo> listaArchivo = buscarArchivosSolicitud(solicitudId);
				obtener.setListaSolicitudArchivo(listaArchivo);
			}
			response.setPayload(obtener);
			response.getStatus().setSuccess(Boolean.TRUE);
		}
		return response;
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<SolicitudPersona> actualizarUbicacionPersona(ReqBase<UbicacionPersonaDTO> request,
			MyJsonWebToken token, Long solicitudPersonaId) {
		RespBase<SolicitudPersona> response = new RespBase<>();
		Optional<SolicitudPersona> solicitudPersonaFind = solicitudPersonaRepository.findById(solicitudPersonaId);
		if (solicitudPersonaFind.isPresent()) {
			SolicitudPersona solicitudPersona = solicitudPersonaFind.get();
			solicitudPersona.setUbigeoId(request.getPayload().getUbigeoId());
			solicitudPersona.setDireccionCompleta(request.getPayload().getDireccionCompleta());
			solicitudPersona.setReferenciaDireccion(request.getPayload().getReferenciaDireccion());
			solicitudPersonaRepository.save(solicitudPersona);
			response.setPayload(solicitudPersona);
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,"NO SE ENCONTRO LA SOLICITUD PERSONA INGRESADA");
		}
		return response;
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespObtenerCorreo> validarCorreoSolicitud(String correo) {
		Random r = new Random();
		
		String valorDado = String.format("%06d", r.nextInt(999999));
		enviarCorreoConfirmacion(correo, Constantes.PLANTILLA_CONFIRMACION_DE_CORREO,valorDado);
		RespObtenerCorreo respPayload = new RespObtenerCorreo();
		respPayload.setCodigoConfirmacion(valorDado);
		return new RespBase<RespObtenerCorreo>().ok(respPayload);
	}
	
	public void enviarNotificacionCambioPass(String correoEnvio, String plantilla) {			
		notificacionService.enviarNotificacionCambioPass(Constantes.ASUNTO_NOTIFICACION_CAMBIO_PWD, plantilla, correoEnvio, true);
	}
	
	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespObtenerCorreo> notificarCorreoCambioPass(String correo) {
		enviarNotificacionCambioPass(correo, Constantes.PLANTILLA_NOTIFICACION_CAMBIO_PWD);
		RespObtenerCorreo respPayload = new RespObtenerCorreo();
		return new RespBase<RespObtenerCorreo>().ok(respPayload);
	}

	public String saveFileToNgnxToReturnRoute(MultipartFile file) throws IOException {

		Parametro parametro = generalRepositorySP.buscarParametro(null, null,Constantes.RUTA_FILE_SERVER_ENTIDAD);
		String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto()).substring(1);

		//obtener el dia actual:
		LocalDate fechaActual = LocalDate.now();
		String diaActual = fechaActual.format(DateTimeFormatter.ofPattern("dd"));

		rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD,diaActual);
		if (parametro != null) {
			RespBase<ApiUploadFile> requestApiUploadFile = new RespBase<>();

				ApiUploadFile uploadFile = new ApiUploadFile();
				uploadFile.setExtension("." + ParametrosUtil.extension(Objects.requireNonNull(file.getOriginalFilename())));
				uploadFile.setFileBase64(Base64.getEncoder().withoutPadding().encodeToString(file.getBytes()));
				uploadFile.setFileName(ParametrosUtil.onlyName(file.getOriginalFilename()));
				uploadFile.setPath(rutaFileServer + "/");
				requestApiUploadFile.setPayload(uploadFile);
				RespBase<RespUploadFile> responseWS = maestraApiClient.uploadFile(requestApiUploadFile);
				if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
					return null;
				}
			return uploadFile.getPath() + uploadFile.getFileName() + uploadFile.getExtension();

		}else{
			return null;
		}

	}
}
