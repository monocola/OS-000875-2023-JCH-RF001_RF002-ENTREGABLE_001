package pe.gob.servir.entidad.service.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import pe.gob.servir.entidad.adapter.BeanAdapterOrganigrama;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.model.BuscarUnidadOrganica;
import pe.gob.servir.entidad.model.ComboByOrganigrama;
import pe.gob.servir.entidad.model.ComboUnidadOrganica;
import pe.gob.servir.entidad.model.DetUnidadOrganica;
import pe.gob.servir.entidad.model.ExisteOrganigramaDTO;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.GestionOrganigramaDTO;
import pe.gob.servir.entidad.model.ListarOrganigramaDTO;
import pe.gob.servir.entidad.model.ListarOrganigramaDTOTest;
import pe.gob.servir.entidad.model.Organigrama;
import pe.gob.servir.entidad.model.OrganigramaDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.ServidorCivilDTO;
import pe.gob.servir.entidad.repository.DetUnidadOrganicaRepository;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.OrganigramaRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.OrganigramaExcelDTO;
import pe.gob.servir.entidad.request.dto.OrganoExcelDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespBuscarUnidadOrganica;
import pe.gob.servir.entidad.response.RespComboPerByOrganigrama;
import pe.gob.servir.entidad.response.RespComboUnidadOrganica;
import pe.gob.servir.entidad.response.RespListaOrganigrama;
import pe.gob.servir.entidad.response.RespListaOrgano;
import pe.gob.servir.entidad.response.RespObtenerGestionOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerLtaOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerServidorCivil;
import pe.gob.servir.entidad.response.RespObtenerValidaOrganigrama;
import pe.gob.servir.entidad.response.RespOrganigrama;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.OrganigramaService;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class OrganigramaServiceImpl implements OrganigramaService {

	private static final Logger LOGGER = Logger.getLogger(OrganigramaServiceImpl.class);

	@Autowired
	private BeanAdapterOrganigrama beanAdapterOrganigrama;

	@Autowired
	private OrganigramaRepository organigramaRepository;

	@Autowired
	private PersonaService personaService;

	@Autowired
	private GestionRepository gestionRepository;

	@Autowired
	private VariablesSistema variablesSistema;

	@Autowired
	private GeneralRepository generalRepository;
	
	@Autowired
	private DetUnidadOrganicaRepository detUnidadOrganicaRepository;

	@Transactional(transactionManager = "entidadTransactionManager")
	@SuppressWarnings("rawtypes")
	@Override
	public RespBase<RespOrganigrama> guardarOrganigrama(ReqBase<ReqOrganigrama> request, MyJsonWebToken token,
			Long organigramaId) {// NOSONAR
		RespBase<RespOrganigrama> response = new RespBase<>();
		Organigrama organigrama = null;
		List<OrganigramaDTO> ltaValid = gestionRepository.buscarOrganigramaByPersona(
				request.getPayload().getTipoDocumento(), request.getPayload().getNumeroDocumento());
		if (organigramaId != null) {
			Optional<Organigrama> organimgramaFind = organigramaRepository.findById(organigramaId);
			if (organimgramaFind.isPresent()) {
				if (!ltaValid.isEmpty()) {
					if (ltaValid.get(0).getEntidadId().longValue() != organimgramaFind.get().getEntidadId()
							.longValue()) {// NOSONAR
						return ParametrosUtil.setearResponse(response, Boolean.FALSE,
								"Ya existe un Representante con el Nro Documento Ingresado.!");

					}
				}
				organigrama = organimgramaFind.get();
				organigrama.setCampoSegUpd(request.getPayload().getEstadoRegistro(), token.getUsuario().getUsuario(),
						Instant.now());
			} else {
				return ParametrosUtil.setearResponse(response, Boolean.FALSE,
						Constantes.MSG_NO_EXISTE_EL_ORGANIGRAMAID_INGRESADO);
			}
		} else {
			if (!ltaValid.isEmpty()) {
				return ParametrosUtil.setearResponse(response, Boolean.FALSE,
						"Ya existe un Representante con el Nro Documento Ingresado.!");
			}
			organigrama = new Organigrama();
			organigrama.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
		}
		PersonaDTO personaResponsable = beanAdapterOrganigrama.adapToPersonaDTO(request);
		if (request.getPayload().getTipoDocumento().intValue() == variablesSistema.tipoDocumentoDni) {
			personaResponsable.setPaisId(variablesSistema.idPaisPeru);
		} else {
			personaResponsable.setPaisId(request.getPayload().getPaisId());
		}
		RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
		ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();
		ParametrosUtil.setearPersonaNatural(apiPersonaNatural, personaResponsable);
		personaNaturalResquest.setPayload(apiPersonaNatural);
		RespBase<Object> responseWS = personaService.obtenerInsertarPersonaNuevaVersion(
				variablesSistema.tipoPersonaNatural, personaResponsable.getTipoDocumento(),
				personaResponsable.getNumeroDocumento(), personaNaturalResquest);
		if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					"El servicio de la API Persona no responde, por favor verifique que el servicio este funcionando correctamente o que los Nombres y Apellidos esten en mayusculas y no tengan numeros");
			return response;
		}
		RespApiPersona personaResponse = beanAdapterOrganigrama.adaptToPersonaResponse(request, organigrama,
				responseWS);
		organigramaRepository.save(organigrama);
		RespOrganigrama payload = new RespOrganigrama();
		payload.setOrganigrama(organigrama);
		payload.setPersona(personaResponse);
		return new RespBase<RespOrganigrama>().ok(payload);
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespOrganigrama> registrarOrganigrama(ReqBase<ReqOrganigrama> request, MyJsonWebToken token,
			Long organigramaId) {// NOSONAR
		RespBase<RespOrganigrama> response = new RespBase<>();
		Organigrama organigrama = null;
		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put("entidadId", request.getPayload().getEntidadId());
		parametroMap.put("sigla", request.getPayload().getSigla());

		if (organigramaId != null) {
			Optional<Organigrama> organimgramaFind = organigramaRepository.findById(organigramaId);
			if (organimgramaFind.isPresent()) {
				organigrama = organimgramaFind.get();
				organigrama.setCampoSegUpd(request.getPayload().getEstadoRegistro(), token.getUsuario().getUsuario(),
						Instant.now());
			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						Constantes.MSG_NO_EXISTE_EL_ORGANIGRAMAID_INGRESADO);
				return response;
			}
		} else {
			organigrama = new Organigrama();
			organigrama.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
		}

		organigrama.setDescripcion(request.getPayload().getDescripcion());
		organigrama.setEntidadId(request.getPayload().getEntidadId());
		organigrama.setNaturalezaOrgano(request.getPayload().getNaturalezaOrgano());
		organigrama.setPadreOrganigramaId(request.getPayload().getPadreOrganigramaId());
		organigrama.setSigla(request.getPayload().getSigla());
		organigrama.setEstadoRegistro(request.getPayload().getEstadoRegistro());
		organigrama.setTipoOrganoUoId(request.getPayload().getTipoOrganoUoId());
		organigramaRepository.save(organigrama);
		RespOrganigrama payload = new RespOrganigrama();
		payload.setOrganigrama(organigrama);
		return new RespBase<RespOrganigrama>().ok(payload);
	}

	@Override
	public RespBase<RespObtenerValidaOrganigrama> verificarDuplicidadOrganigrama(Map<String, Object> parametroMap) {
		GenericResponseMessage valida = validaDuplicidad(parametroMap);
		RespObtenerValidaOrganigrama respPayload = new RespObtenerValidaOrganigrama();
		respPayload.setValidaOrganigrama(valida);
		return new RespBase<RespObtenerValidaOrganigrama>().ok(respPayload);
	}

	private GenericResponseMessage validaDuplicidad(Map<String, Object> parametroMap) {
		List<ExisteOrganigramaDTO> lista = gestionRepository.obtenerValidaOrganigrama(parametroMap);
		GenericResponseMessage valida = new GenericResponseMessage();
		if (lista != null && !lista.isEmpty()) {
			String descripcion = (String) parametroMap.get("descripcion");
			String sigla = (String) parametroMap.get("sigla");
			if (descripcion != null && !descripcion.trim().isEmpty()) {
				String valDescripcion = (String) lista.get(0).getDescripcion();
				if (valDescripcion != null && !valDescripcion.trim().isEmpty()) {
					valida.setCodigo(1L);
					valida.setMensaje("Este valor ya se encuentra registrado");
				} else {
					valida.setCodigo(0L);
					valida.setMensaje("");
				}
			} else if (sigla != null && !sigla.trim().isEmpty()) {
				String valSigla = (String) lista.get(0).getSigla();
				if (valSigla != null && !valSigla.trim().isEmpty()) {
					valida.setCodigo(1L);
					valida.setMensaje("Este valor ya se encuentra registrado");
				} else {
					valida.setCodigo(0L);
					valida.setMensaje("");
				}
			} else {
				valida.setCodigo(0L);
				valida.setMensaje("");
			}
		} else {
			valida.setCodigo(0L);
			valida.setMensaje("");
		}
		return valida;
	}

	@Override
	public RespBase<RespObtenerOrganigrama> buscarOrganigramaByFilter(Map<String, Object> parametroMap) {	
		List<OrganigramaDTO> lista = gestionRepository.buscarOrganigramaByFilter(parametroMap);	
		RespObtenerOrganigrama respPayload = new RespObtenerOrganigrama();
		List<OrganigramaDTO> listas =  new ArrayList<>();
		lista.stream().forEach(dto -> {

			if (Objects.nonNull(dto.getUrlFoto())) {
				dto.setUrlFoto(variablesSistema.fileServer + dto.getUrlFoto());
			}
			listas.add(dto);

		});
		respPayload.setListaOrganigrama(listas);
		return new RespBase<RespObtenerOrganigrama>().ok(respPayload);
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> eliminarOrganigrama(MyJsonWebToken token, Long organigramaId, String estado) {
		RespBase<Object> response = new RespBase<>();
		Optional<Organigrama> organimgramaFind = organigramaRepository.findById(organigramaId);
		if (organimgramaFind.isPresent()) {
			Organigrama organigrama = organimgramaFind.get();
			if (estado.equals(EstadoRegistro.INACTIVO.getCodigo())) {
				Organigrama organigramaFilter = new Organigrama();
				organigramaFilter.setPadreOrganigramaId(organigrama.getOrganigramaId());
				organigramaFilter.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
				Example<Organigrama> example = Example.of(organigramaFilter);
				List<Organigrama> ltaOrganigramaFilter = organigramaRepository.findAll(example);
				if (!ltaOrganigramaFilter.isEmpty()) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							"No se puede eliminar, aún tiene órganos dependientes");
					return response;
				}
			}
			organigrama.setEstadoRegistro(estado);
			organigrama.setCampoSegUpd(EstadoRegistro.INACTIVO.getCodigo(), token.getUsuario().getUsuario(),
					Instant.now());
			organigramaRepository.save(organigrama);
			return new RespBase<Object>().ok(organigrama);
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					Constantes.MSG_NO_EXISTE_EL_ORGANIGRAMAID_INGRESADO);
		}
		return response;
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> eliminarGestionOrganigrama(MyJsonWebToken token, Long organigramaId, String estado) {
		RespBase<Object> response = new RespBase<>();
		Optional<Organigrama> organimgramaFind = organigramaRepository.findById(organigramaId);
		if (organimgramaFind.isPresent()) {
			Organigrama organigrama = organimgramaFind.get();
			if (estado.equals(EstadoRegistro.INACTIVO.getCodigo())) {
				Organigrama organigramaFilter = new Organigrama();
				organigramaFilter.setPadreOrganigramaId(organigrama.getOrganigramaId());
				organigramaFilter.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
				Example<Organigrama> example = Example.of(organigramaFilter);
				List<Organigrama> ltaOrganigramaFilter = organigramaRepository.findAll(example);
				if (!ltaOrganigramaFilter.isEmpty()) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							"la Unidad Orgánica tiene dependencias");
					return response;
				}
				List<DetUnidadOrganica> listaDetaUO =  detUnidadOrganicaRepository.findByOrganigramaIdAndEstadoRegistro(organigramaId, Constantes.ESTADO_ACTIVO);
				if (listaDetaUO.size() > 0) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							"El siguiente registro a eliminar cuenta con servidores asignados, le recomendamos, modificar la asignación de estos para proceder a la siguiente acción");
					return response;
				}
			}
			organigrama.setEstadoRegistro(estado);
			organigrama.setCampoSegUpd(EstadoRegistro.INACTIVO.getCodigo(), token.getUsuario().getUsuario(),
					Instant.now());
			organigramaRepository.save(organigrama);
			return new RespBase<Object>().ok(organigrama);
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					Constantes.MSG_NO_EXISTE_EL_ORGANIGRAMAID_INGRESADO);
		}
		return response;
	}

	@Override
	public RespBase<RespObtenerLtaOrganigrama> buscarOrganigramaByTipo(Long tipo, Long entidadId) {
		Organigrama organigramaFilter = new Organigrama();
		organigramaFilter.setTipoOrganoUoId(tipo);
		organigramaFilter.setEntidadId(entidadId);
		organigramaFilter.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
		Example<Organigrama> example = Example.of(organigramaFilter);
		List<Organigrama> ltaOrganigramaFilter = organigramaRepository.findAll(example);
		RespObtenerLtaOrganigrama respPayload = new RespObtenerLtaOrganigrama();
		respPayload.setListaOrganigrama(ltaOrganigramaFilter);
		return new RespBase<RespObtenerLtaOrganigrama>().ok(respPayload);
	}

	@Override
	public RespBase<RespComboUnidadOrganica> buscarUnidadesOrganicasPorEntidad(Map<String, Object> parametroMap) {
		List<ComboUnidadOrganica> ltaUnidadOrganicaFilter = gestionRepository.buscarUnidadesOrganicas(parametroMap);
		RespComboUnidadOrganica respPayload = new RespComboUnidadOrganica();
		respPayload.setListaComboUnidadOrganica(ltaUnidadOrganicaFilter);
		return new RespBase<RespComboUnidadOrganica>().ok(respPayload);
	}

	@Override
	public RespBase<RespBuscarUnidadOrganica> buscarUnidadOrganicaSuperior(Map<String, Object> parametroMap) {
		List<BuscarUnidadOrganica> ltaUnidadOrganicaFilter = gestionRepository.buscarUnidadesOrganicasSuperior(parametroMap);
		RespBuscarUnidadOrganica respPayload = new RespBuscarUnidadOrganica();
		respPayload.setListaComboUnidadOrganica(ltaUnidadOrganicaFilter);
		return new RespBase<RespBuscarUnidadOrganica>().ok(respPayload);
	}

	public RespBase<RespListaOrgano> buscarOrganigramaV2(Map<String, Object> parametroMap) {
		RespListaOrgano respPayload = new RespListaOrgano();
		List<RespListaOrgano.OrganigramaPadre> listaOrgano = new ArrayList<>();
		List<ListarOrganigramaDTOTest> listaOrganoPadre = gestionRepository.buscarOrganoPadre(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijo = gestionRepository.buscarOrganoHijo(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijoA = gestionRepository.buscarOrganoHijo(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijoB = gestionRepository.buscarOrganoHijo(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijoC = gestionRepository.buscarOrganoHijo(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijoD = gestionRepository.buscarOrganoHijo(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijoE = gestionRepository.buscarOrganoHijo(parametroMap);
		List<ListarOrganigramaDTOTest> listaOrganoHijoF = gestionRepository.buscarOrganoHijo(parametroMap);
		generarListaOrganos(listaOrganoPadre, listaOrganoHijo, listaOrganoHijoA, listaOrganoHijoB, listaOrganoHijoC,
				listaOrganoHijoD, listaOrganoHijoE, listaOrganoHijoF, listaOrgano);
		respPayload.setListaOrganigrama(listaOrgano);
		return new RespBase<RespListaOrgano>().ok(respPayload);
	}

	@Override
	public RespBase<RespListaOrganigrama> buscarOrganigramas(Map<String, Object> parametroMap) {
		RespListaOrganigrama respPayload = new RespListaOrganigrama();
		List<RespListaOrganigrama.OrganigramaPadre> listaOrganigrama = new ArrayList<>();
		List<ListarOrganigramaDTO> listaOrganigramaPadre = gestionRepository.buscarOrganigramaPadre(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijo = gestionRepository.buscarOrganigramaHijo(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijoA = gestionRepository.buscarOrganigramaHijo(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijoB = gestionRepository.buscarOrganigramaHijo(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijoC = gestionRepository.buscarOrganigramaHijo(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijoD = gestionRepository.buscarOrganigramaHijo(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijoE = gestionRepository.buscarOrganigramaHijo(parametroMap);
		List<ListarOrganigramaDTO> listaOrganigramaHijoF = gestionRepository.buscarOrganigramaHijo(parametroMap);
		generarListaOrganigramas(listaOrganigramaPadre, listaOrganigramaHijo, listaOrganigramaHijoA,
				listaOrganigramaHijoB, listaOrganigramaHijoC, listaOrganigramaHijoD, listaOrganigramaHijoE,
				listaOrganigramaHijoF, listaOrganigrama);
		respPayload.setListaOrganigrama(listaOrganigrama);
		return new RespBase<RespListaOrganigrama>().ok(respPayload);
	}

	private void generarListaOrganigramas(List<ListarOrganigramaDTO> listaOrganigrama,
			List<ListarOrganigramaDTO> listaOrganigramaHijo, // NOSONAR
			List<ListarOrganigramaDTO> listaOrganigramaHijoA, List<ListarOrganigramaDTO> listaOrganigramaHijoB,
			List<ListarOrganigramaDTO> listaOrganigramaHijoC, List<ListarOrganigramaDTO> listaOrganigramaHijoD,
			List<ListarOrganigramaDTO> listaOrganigramaHijoE, List<ListarOrganigramaDTO> listaOrganigramaHijoF,
			List<RespListaOrganigrama.OrganigramaPadre> responseOrganigramas) {
		RespListaOrganigrama.OrganigramaPadre organigrama;
		if (listaOrganigrama.isEmpty()) {
			Collections.sort(listaOrganigramaHijo, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
				@Override
				public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {
					return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());
				}
			});
			for (int i = listaOrganigramaHijo.size() - 1; i >= 0; i--) {
				if (listaOrganigramaHijo.size() > 1) {
					listaOrganigramaHijo.remove(i);
				}
			}

			for (ListarOrganigramaDTO organigramaHijo : listaOrganigramaHijo) {

				organigrama = new RespListaOrganigrama.OrganigramaPadre();
				organigrama.setIdOrganigrama(organigramaHijo.getIdOrganigrama());
				organigrama.setIdEntidad(organigramaHijo.getIdEntidad());

				organigrama.setAreaId(organigramaHijo.getAreaId());
				organigrama.setSedeId(organigramaHijo.getSedeId());
				organigrama.setPuesto(organigramaHijo.getPuesto());
				organigrama.setOrden(organigramaHijo.getOrden());
				organigrama.setDescripcion(organigramaHijo.getDescripcion());
				organigrama.setNivelId(organigramaHijo.getNivelId());
				organigrama.setDesNivel(organigramaHijo.getDesNivel());
				organigrama.setSigla(organigramaHijo.getSigla());
				organigrama.setNaturalezaId(organigramaHijo.getNaturalezaId());
				organigrama.setDesNaturaleza(organigramaHijo.getDesNaturaleza());
				organigrama.setEstadoId(organigramaHijo.getEstadoId());
				organigrama.setEstado(organigramaHijo.getEstado());
				organigrama.setPadreIdHijo(organigramaHijo.getPadreIdOrgHijo());
				organigrama.setTipoOrganoId(organigramaHijo.getTipoOrganoId());
				organigrama.setDesTipoOrgano(organigramaHijo.getDesTipoOrgano());
				organigrama.setNivelGobiernoId(organigramaHijo.getNivelGobiernoId());
				organigrama.setDescripcionCorta(organigramaHijo.getDescripcionCorta());
				organigrama.setPersonaResponsableId(organigramaHijo.getPersonaResponsableId());
				organigrama.setNombres(organigramaHijo.getNombres());
				organigrama.setApellidoPaterno(organigramaHijo.getApellidoPaterno());
				organigrama.setApellidoMaterno(organigramaHijo.getApellidoMaterno());
				organigrama.setTipoDocumentoId(organigramaHijo.getTipoDocumentoId());
				organigrama.setTipoDocumento(organigramaHijo.getTipoDocumento());
				organigrama.setNumeroDocumento(organigramaHijo.getNumeroDocumento());
				organigrama.setTelefonoId(organigramaHijo.getTelefonoId());
				organigrama.setTelefono(organigramaHijo.getTelefono());
				organigrama.setCorreoId(organigramaHijo.getCorreoId());
				organigrama.setCorreo(organigramaHijo.getCorreo());
				organigrama.setPaisId(organigramaHijo.getPaisId());
				organigrama.setNombrePais(organigramaHijo.getNombrePais());
				organigrama.setPadreId(organigramaHijo.getPadreId());
				List<RespListaOrganigrama.OrganigramaHijo> listOrganigramaHijoA = new ArrayList<>();
				Collections.sort(listaOrganigramaHijoA, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
					@Override
					public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {
						return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());
					}
				});
				for (ListarOrganigramaDTO organigramaHijoA : listaOrganigramaHijoA) {
					RespListaOrganigrama.OrganigramaHijo hijoA;
					if (organigramaHijo.getIdOrganigrama().toString()
							.equals(organigramaHijoA.getPadreIdOrgHijo().toString())) {
						hijoA = new RespListaOrganigrama.OrganigramaHijo();
						hijoA.setIdOrganigrama(organigramaHijoA.getIdOrganigrama());
						hijoA.setIdEntidad(organigramaHijoA.getIdEntidad());
						hijoA.setAreaId(organigramaHijoA.getAreaId());
						hijoA.setSedeId(organigramaHijoA.getSedeId());
						hijoA.setPuesto(organigramaHijoA.getPuesto());
						hijoA.setOrden(organigramaHijoA.getOrden());
						hijoA.setDescripcion(organigramaHijoA.getDescripcion());
						hijoA.setNivelId(organigramaHijoA.getNivelId());
						hijoA.setDesNivel(organigramaHijoA.getDesNivel());
						hijoA.setSigla(organigramaHijoA.getSigla());
						hijoA.setNaturalezaId(organigramaHijoA.getNaturalezaId());
						hijoA.setDesNaturaleza(organigramaHijoA.getDesNaturaleza());
						hijoA.setEstadoId(organigramaHijoA.getEstadoId());
						hijoA.setEstado(organigramaHijoA.getEstado());
						hijoA.setPadreIdHijo(organigramaHijoA.getPadreIdOrgHijo());
						hijoA.setTipoOrganoId(organigramaHijoA.getTipoOrganoId());
						hijoA.setDesTipoOrgano(organigramaHijoA.getDesTipoOrgano());
						hijoA.setNivelGobiernoId(organigramaHijoA.getNivelGobiernoId());
						hijoA.setDescripcionCorta(organigramaHijoA.getDescripcionCorta());
						hijoA.setPersonaResponsableId(organigramaHijoA.getPersonaResponsableId());
						hijoA.setNombres(organigramaHijoA.getNombres());
						hijoA.setApellidoPaterno(organigramaHijoA.getApellidoPaterno());
						hijoA.setApellidoMaterno(organigramaHijoA.getApellidoMaterno());
						hijoA.setTipoDocumentoId(organigramaHijoA.getTipoDocumentoId());
						hijoA.setTipoDocumento(organigramaHijoA.getTipoDocumento());
						hijoA.setNumeroDocumento(organigramaHijoA.getNumeroDocumento());
						hijoA.setTelefonoId(organigramaHijoA.getTelefonoId());
						hijoA.setTelefono(organigramaHijoA.getTelefono());
						hijoA.setCorreoId(organigramaHijoA.getCorreoId());
						hijoA.setCorreo(organigramaHijoA.getCorreo());
						hijoA.setPaisId(organigramaHijoA.getPaisId());
						hijoA.setNombrePais(organigramaHijoA.getNombrePais());
						hijoA.setPadreId(organigramaHijoA.getPadreId());
						List<RespListaOrganigrama.OrganigramaHijoA> listOrganigramaHijoB = new ArrayList<>();
						Collections.sort(listaOrganigramaHijoB, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
							@Override
							public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {
								return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());
							}
						});
						for (ListarOrganigramaDTO organigramaHijoB : listaOrganigramaHijoB) {
							RespListaOrganigrama.OrganigramaHijoA hijoB = null;
							if (organigramaHijoB.getPadreIdOrgHijo().toString()
									.equals(organigramaHijoA.getIdOrganigrama().toString())) {
								hijoB = new RespListaOrganigrama.OrganigramaHijoA();
								hijoB.setIdOrganigrama(organigramaHijoB.getIdOrganigrama());
								hijoB.setIdEntidad(organigramaHijoB.getIdEntidad());
								hijoB.setAreaId(organigramaHijoB.getAreaId());
								hijoB.setSedeId(organigramaHijoB.getSedeId());
								hijoB.setPuesto(organigramaHijoB.getPuesto());
								hijoB.setOrden(organigramaHijoB.getOrden());
								hijoB.setDescripcion(organigramaHijoB.getDescripcion());
								hijoB.setNivelId(organigramaHijoB.getNivelId());
								hijoB.setDesNivel(organigramaHijoB.getDesNivel());
								hijoB.setSigla(organigramaHijoB.getSigla());
								hijoB.setNaturalezaId(organigramaHijoB.getNaturalezaId());
								hijoB.setDesNaturaleza(organigramaHijoB.getDesNaturaleza());
								hijoB.setEstadoId(organigramaHijoB.getEstadoId());
								hijoB.setEstado(organigramaHijoB.getEstado());
								hijoB.setPadreIdHijo(organigramaHijoB.getPadreIdOrgHijo());
								hijoB.setTipoOrganoId(organigramaHijoB.getTipoOrganoId());
								hijoB.setDesTipoOrgano(organigramaHijoB.getDesTipoOrgano());
								hijoB.setNivelGobiernoId(organigramaHijoB.getNivelGobiernoId());
								hijoB.setDescripcionCorta(organigramaHijoB.getDescripcionCorta());
								hijoB.setPersonaResponsableId(organigramaHijoB.getPersonaResponsableId());
								hijoB.setNombres(organigramaHijoB.getNombres());
								hijoB.setApellidoPaterno(organigramaHijoB.getApellidoPaterno());
								hijoB.setApellidoMaterno(organigramaHijoB.getApellidoMaterno());
								hijoB.setTipoDocumentoId(organigramaHijoB.getTipoDocumentoId());
								hijoB.setTipoDocumento(organigramaHijoB.getTipoDocumento());
								hijoB.setNumeroDocumento(organigramaHijoB.getNumeroDocumento());
								hijoB.setTelefonoId(organigramaHijoB.getTelefonoId());
								hijoB.setTelefono(organigramaHijoB.getTelefono());
								hijoB.setCorreoId(organigramaHijoB.getCorreoId());
								hijoB.setCorreo(organigramaHijoB.getCorreo());
								hijoB.setPaisId(organigramaHijoB.getPaisId());
								hijoB.setNombrePais(organigramaHijoB.getNombrePais());
								hijoB.setPadreId(organigramaHijoB.getPadreId());
								listOrganigramaHijoB.add(hijoB);
							}
							hijoA.setListaOrganigramaHijoA(listOrganigramaHijoB);
						}
						listOrganigramaHijoA.add(hijoA);
					}
					organigrama.setListaOrganigramaHijo(listOrganigramaHijoA);
				}
				responseOrganigramas.add(organigrama);
			}
		}
		for (ListarOrganigramaDTO organigramaPadre : listaOrganigrama) {

			organigrama = new RespListaOrganigrama.OrganigramaPadre();
			organigrama.setIdOrganigrama(organigramaPadre.getIdOrganigrama());
			organigrama.setIdEntidad(organigramaPadre.getIdEntidad());

			organigrama.setAreaId(organigramaPadre.getAreaId());
			organigrama.setSedeId(organigramaPadre.getSedeId());
			organigrama.setPuesto(organigramaPadre.getPuesto());
			organigrama.setOrden(organigramaPadre.getOrden());
			organigrama.setDescripcion(organigramaPadre.getDescripcion());
			organigrama.setNivelId(organigramaPadre.getNivelId());
			organigrama.setDesNivel(organigramaPadre.getDesNivel());
			organigrama.setSigla(organigramaPadre.getSigla());
			organigrama.setNaturalezaId(organigramaPadre.getNaturalezaId());
			organigrama.setDesNaturaleza(organigramaPadre.getDesNaturaleza());
			organigrama.setEstadoId(organigramaPadre.getEstadoId());
			organigrama.setEstado(organigramaPadre.getEstado());
			organigrama.setPadreIdHijo(organigramaPadre.getPadreIdOrgHijo());
			organigrama.setTipoOrganoId(organigramaPadre.getTipoOrganoId());
			organigrama.setDesTipoOrgano(organigramaPadre.getDesTipoOrgano());
			organigrama.setNivelGobiernoId(organigramaPadre.getNivelGobiernoId());
			organigrama.setDescripcionCorta(organigramaPadre.getDescripcionCorta());
			organigrama.setPersonaResponsableId(organigramaPadre.getPersonaResponsableId());
			organigrama.setNombres(organigramaPadre.getNombres());
			organigrama.setApellidoPaterno(organigramaPadre.getApellidoPaterno());
			organigrama.setApellidoMaterno(organigramaPadre.getApellidoMaterno());
			organigrama.setTipoDocumentoId(organigramaPadre.getTipoDocumentoId());
			organigrama.setTipoDocumento(organigramaPadre.getTipoDocumento());
			organigrama.setNumeroDocumento(organigramaPadre.getNumeroDocumento());
			organigrama.setTelefonoId(organigramaPadre.getTelefonoId());
			organigrama.setTelefono(organigramaPadre.getTelefono());
			organigrama.setCorreoId(organigramaPadre.getCorreoId());
			organigrama.setCorreo(organigramaPadre.getCorreo());
			organigrama.setPaisId(organigramaPadre.getPaisId());
			organigrama.setNombrePais(organigramaPadre.getNombrePais());
			organigrama.setPadreId(organigramaPadre.getPadreId());
			List<RespListaOrganigrama.OrganigramaHijo> listOrganigramaHijo = new ArrayList<>();
			Collections.sort(listaOrganigramaHijo, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
				@Override
				public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {
					return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());
				}
			});
			for (ListarOrganigramaDTO organigramaHijo : listaOrganigramaHijo) {
				RespListaOrganigrama.OrganigramaHijo hijo;
				if (organigramaPadre.getIdOrganigrama().toString()
						.equals(organigramaHijo.getPadreIdOrgHijo().toString())) {
					hijo = new RespListaOrganigrama.OrganigramaHijo();
					hijo.setIdOrganigrama(organigramaHijo.getIdOrganigrama());
					hijo.setIdEntidad(organigramaHijo.getIdEntidad());
					hijo.setAreaId(organigramaHijo.getAreaId());
					hijo.setSedeId(organigramaHijo.getSedeId());
					hijo.setPuesto(organigramaHijo.getPuesto());
					hijo.setOrden(organigramaHijo.getOrden());
					hijo.setDescripcion(organigramaHijo.getDescripcion());
					hijo.setNivelId(organigramaHijo.getNivelId());
					hijo.setDesNivel(organigramaHijo.getDesNivel());
					hijo.setSigla(organigramaHijo.getSigla());
					hijo.setNaturalezaId(organigramaHijo.getNaturalezaId());
					hijo.setDesNaturaleza(organigramaHijo.getDesNaturaleza());
					hijo.setEstadoId(organigramaHijo.getEstadoId());
					hijo.setEstado(organigramaHijo.getEstado());
					hijo.setPadreIdHijo(organigramaHijo.getPadreIdOrgHijo());
					hijo.setTipoOrganoId(organigramaHijo.getTipoOrganoId());
					hijo.setDesTipoOrgano(organigramaHijo.getDesTipoOrgano());
					hijo.setNivelGobiernoId(organigramaHijo.getNivelGobiernoId());
					hijo.setDescripcionCorta(organigramaHijo.getDescripcionCorta());
					hijo.setPersonaResponsableId(organigramaHijo.getPersonaResponsableId());
					hijo.setNombres(organigramaHijo.getNombres());
					hijo.setApellidoPaterno(organigramaHijo.getApellidoPaterno());
					hijo.setApellidoMaterno(organigramaHijo.getApellidoMaterno());
					hijo.setTipoDocumentoId(organigramaHijo.getTipoDocumentoId());
					hijo.setTipoDocumento(organigramaHijo.getTipoDocumento());
					hijo.setNumeroDocumento(organigramaHijo.getNumeroDocumento());
					hijo.setTelefonoId(organigramaHijo.getTelefonoId());
					hijo.setTelefono(organigramaHijo.getTelefono());
					hijo.setCorreoId(organigramaHijo.getCorreoId());
					hijo.setCorreo(organigramaHijo.getCorreo());
					hijo.setPaisId(organigramaHijo.getPaisId());
					hijo.setNombrePais(organigramaHijo.getNombrePais());
					hijo.setPadreId(organigramaHijo.getPadreId());
					List<RespListaOrganigrama.OrganigramaHijoA> listOrganigramaHijoA = new ArrayList<>();
					Collections.sort(listaOrganigramaHijoA, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
						@Override
						public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {
							return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());
						}
					});
					for (ListarOrganigramaDTO organigramaHijoA : listaOrganigramaHijoA) {
						RespListaOrganigrama.OrganigramaHijoA hijoA = null;
						if (organigramaHijoA.getPadreIdOrgHijo().toString()
								.equals(organigramaHijo.getIdOrganigrama().toString())) {
							hijoA = new RespListaOrganigrama.OrganigramaHijoA();
							hijoA.setIdOrganigrama(organigramaHijoA.getIdOrganigrama());
							hijoA.setIdEntidad(organigramaHijoA.getIdEntidad());
							hijoA.setAreaId(organigramaHijoA.getAreaId());
							hijoA.setSedeId(organigramaHijoA.getSedeId());
							hijoA.setPuesto(organigramaHijoA.getPuesto());
							hijoA.setOrden(organigramaHijoA.getOrden());
							hijoA.setDescripcion(organigramaHijoA.getDescripcion());
							hijoA.setNivelId(organigramaHijoA.getNivelId());
							hijoA.setDesNivel(organigramaHijoA.getDesNivel());
							hijoA.setSigla(organigramaHijoA.getSigla());
							hijoA.setNaturalezaId(organigramaHijoA.getNaturalezaId());
							hijoA.setDesNaturaleza(organigramaHijoA.getDesNaturaleza());
							hijoA.setEstadoId(organigramaHijoA.getEstadoId());
							hijoA.setEstado(organigramaHijoA.getEstado());
							hijoA.setPadreIdHijo(organigramaHijoA.getPadreIdOrgHijo());
							hijoA.setTipoOrganoId(organigramaHijoA.getTipoOrganoId());
							hijoA.setDesTipoOrgano(organigramaHijoA.getDesTipoOrgano());
							hijoA.setNivelGobiernoId(organigramaHijoA.getNivelGobiernoId());
							hijoA.setDescripcionCorta(organigramaHijoA.getDescripcionCorta());
							hijoA.setPersonaResponsableId(organigramaHijoA.getPersonaResponsableId());
							hijoA.setNombres(organigramaHijoA.getNombres());
							hijoA.setApellidoPaterno(organigramaHijoA.getApellidoPaterno());
							hijoA.setApellidoMaterno(organigramaHijoA.getApellidoMaterno());
							hijoA.setTipoDocumentoId(organigramaHijoA.getTipoDocumentoId());
							hijoA.setTipoDocumento(organigramaHijoA.getTipoDocumento());
							hijoA.setNumeroDocumento(organigramaHijoA.getNumeroDocumento());
							hijoA.setTelefonoId(organigramaHijoA.getTelefonoId());
							hijoA.setTelefono(organigramaHijoA.getTelefono());
							hijoA.setCorreoId(organigramaHijoA.getCorreoId());
							hijoA.setCorreo(organigramaHijoA.getCorreo());
							hijoA.setPaisId(organigramaHijoA.getPaisId());
							hijoA.setNombrePais(organigramaHijoA.getNombrePais());
							hijoA.setPadreId(organigramaHijoA.getPadreId());

							List<RespListaOrganigrama.OrganigramaHijoB> listOrganigramaHijoB = new ArrayList<>();
							Collections.sort(listaOrganigramaHijoB, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
								@Override
								public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {
									return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());
								}
							});
							for (ListarOrganigramaDTO organigramaHijoB : listaOrganigramaHijoB) {
								RespListaOrganigrama.OrganigramaHijoB hijoB = null;
								if (organigramaHijoB.getPadreIdOrgHijo().toString()
										.equals(organigramaHijoA.getIdOrganigrama().toString())) {
									hijoB = new RespListaOrganigrama.OrganigramaHijoB();
									hijoB.setIdOrganigrama(organigramaHijoB.getIdOrganigrama());
									hijoB.setIdEntidad(organigramaHijoB.getIdEntidad());
									hijoB.setAreaId(organigramaHijoB.getAreaId());
									hijoB.setSedeId(organigramaHijoB.getSedeId());
									hijoB.setPuesto(organigramaHijoB.getPuesto());
									hijoB.setOrden(organigramaHijoB.getOrden());
									hijoB.setDescripcion(organigramaHijoB.getDescripcion());
									hijoB.setNivelId(organigramaHijoB.getNivelId());
									hijoB.setDesNivel(organigramaHijoB.getDesNivel());
									hijoB.setSigla(organigramaHijoB.getSigla());
									hijoB.setNaturalezaId(organigramaHijoB.getNaturalezaId());
									hijoB.setDesNaturaleza(organigramaHijoB.getDesNaturaleza());
									hijoB.setEstadoId(organigramaHijoB.getEstadoId());
									hijoB.setEstado(organigramaHijoB.getEstado());
									hijoB.setPadreIdHijo(organigramaHijoB.getPadreIdOrgHijo());
									hijoB.setTipoOrganoId(organigramaHijoB.getTipoOrganoId());
									hijoB.setDesTipoOrgano(organigramaHijoB.getDesTipoOrgano());
									hijoB.setNivelGobiernoId(organigramaHijoB.getNivelGobiernoId());
									hijoB.setDescripcionCorta(organigramaHijoB.getDescripcionCorta());
									hijoB.setPersonaResponsableId(organigramaHijoB.getPersonaResponsableId());
									hijoB.setNombres(organigramaHijoB.getNombres());
									hijoB.setApellidoPaterno(organigramaHijoB.getApellidoPaterno());
									hijoB.setApellidoMaterno(organigramaHijoB.getApellidoMaterno());
									hijoB.setTipoDocumentoId(organigramaHijoB.getTipoDocumentoId());
									hijoB.setTipoDocumento(organigramaHijoB.getTipoDocumento());
									hijoB.setNumeroDocumento(organigramaHijoB.getNumeroDocumento());
									hijoB.setTelefonoId(organigramaHijoB.getTelefonoId());
									hijoB.setTelefono(organigramaHijoB.getTelefono());
									hijoB.setCorreoId(organigramaHijoB.getCorreoId());
									hijoB.setCorreo(organigramaHijoB.getCorreo());
									hijoB.setPaisId(organigramaHijoB.getPaisId());
									hijoB.setNombrePais(organigramaHijoB.getNombrePais());
									hijoB.setPadreId(organigramaHijoB.getPadreId());

									List<RespListaOrganigrama.OrganigramaHijoC> listOrganigramaHijoC = new ArrayList<>();
									Collections.sort(listaOrganigramaHijoC, new Comparator<ListarOrganigramaDTO>() {// NOSONAR
										@Override
										public int compare(ListarOrganigramaDTO p1, ListarOrganigramaDTO p2) {

											return p1.getIdOrganigrama().compareTo(p2.getIdOrganigrama());

										}
									});

									for (ListarOrganigramaDTO organigramaHijoC : listaOrganigramaHijoC) {
										RespListaOrganigrama.OrganigramaHijoC hijoC;
										if (organigramaHijoC.getPadreIdOrgHijo().toString()
												.equals(organigramaHijoB.getIdOrganigrama().toString())) {
											hijoC = new RespListaOrganigrama.OrganigramaHijoC();
											hijoC.setIdOrganigrama(organigramaHijoC.getIdOrganigrama());
											hijoC.setIdEntidad(organigramaHijoC.getIdEntidad());
											hijoC.setAreaId(organigramaHijoC.getAreaId());
											hijoC.setSedeId(organigramaHijoC.getSedeId());
											hijoC.setPuesto(organigramaHijoC.getPuesto());
											hijoC.setOrden(organigramaHijoC.getOrden());
											hijoC.setDescripcion(organigramaHijoC.getDescripcion());
											hijoC.setNivelId(organigramaHijoC.getNivelId());
											hijoC.setDesNivel(organigramaHijoC.getDesNivel());
											hijoC.setSigla(organigramaHijoC.getSigla());
											hijoC.setNaturalezaId(organigramaHijoC.getNaturalezaId());
											hijoC.setDesNaturaleza(organigramaHijoC.getDesNaturaleza());
											hijoC.setEstadoId(organigramaHijoC.getEstadoId());
											hijoC.setEstado(organigramaHijoC.getEstado());
											hijoC.setPadreIdHijo(organigramaHijoC.getPadreIdOrgHijo());
											hijoC.setTipoOrganoId(organigramaHijoC.getTipoOrganoId());
											hijoC.setDesTipoOrgano(organigramaHijoC.getDesTipoOrgano());
											hijoC.setNivelGobiernoId(organigramaHijoC.getNivelGobiernoId());
											hijoC.setDescripcionCorta(organigramaHijoC.getDescripcionCorta());
											hijoC.setPersonaResponsableId(organigramaHijoC.getPersonaResponsableId());
											hijoC.setNombres(organigramaHijoC.getNombres());
											hijoC.setApellidoPaterno(organigramaHijoC.getApellidoPaterno());
											hijoC.setApellidoMaterno(organigramaHijoC.getApellidoMaterno());
											hijoC.setTipoDocumentoId(organigramaHijoC.getTipoDocumentoId());
											hijoC.setTipoDocumento(organigramaHijoC.getTipoDocumento());
											hijoC.setNumeroDocumento(organigramaHijoC.getNumeroDocumento());
											hijoC.setTelefonoId(organigramaHijoC.getTelefonoId());
											hijoC.setTelefono(organigramaHijoC.getTelefono());
											hijoC.setCorreoId(organigramaHijoC.getCorreoId());
											hijoC.setCorreo(organigramaHijoC.getCorreo());
											hijoC.setPaisId(organigramaHijoC.getPaisId());
											hijoC.setNombrePais(organigramaHijoC.getNombrePais());
											hijoC.setPadreId(organigramaHijoC.getPadreId());

											List<RespListaOrganigrama.OrganigramaHijoD> listOrganigramaHijoD = new ArrayList<>();

											Collections.sort(listaOrganigramaHijoD,
													new Comparator<ListarOrganigramaDTO>() {// NOSONAR
														@Override
														public int compare(ListarOrganigramaDTO p1,
																ListarOrganigramaDTO p2) {
															return p1.getIdOrganigrama()
																	.compareTo(p2.getIdOrganigrama());
														}
													});

											for (ListarOrganigramaDTO organigramaHijoD : listaOrganigramaHijoD) {
												RespListaOrganigrama.OrganigramaHijoD hijoD;
												if (organigramaHijoD.getPadreIdOrgHijo().toString()
														.equals(organigramaHijoC.getIdOrganigrama().toString())) {
													hijoD = new RespListaOrganigrama.OrganigramaHijoD();
													hijoD.setIdOrganigrama(organigramaHijoD.getIdOrganigrama());
													hijoD.setIdEntidad(organigramaHijoD.getIdEntidad());
													hijoD.setAreaId(organigramaHijoD.getAreaId());
													hijoD.setSedeId(organigramaHijoD.getSedeId());
													hijoD.setPuesto(organigramaHijoD.getPuesto());
													hijoD.setOrden(organigramaHijoD.getOrden());
													hijoD.setDescripcion(organigramaHijoD.getDescripcion());
													hijoD.setNivelId(organigramaHijoD.getNivelId());
													hijoD.setDesNivel(organigramaHijoD.getDesNivel());
													hijoD.setSigla(organigramaHijoD.getSigla());
													hijoD.setNaturalezaId(organigramaHijoD.getNaturalezaId());
													hijoD.setDesNaturaleza(organigramaHijoD.getDesNaturaleza());
													hijoD.setEstadoId(organigramaHijoD.getEstadoId());
													hijoD.setEstado(organigramaHijoD.getEstado());
													hijoD.setPadreIdHijo(organigramaHijoD.getPadreIdOrgHijo());
													hijoD.setTipoOrganoId(organigramaHijoD.getTipoOrganoId());
													hijoD.setDesTipoOrgano(organigramaHijoD.getDesTipoOrgano());
													hijoD.setNivelGobiernoId(organigramaHijoD.getNivelGobiernoId());
													hijoD.setDescripcionCorta(organigramaHijoD.getDescripcionCorta());
													hijoD.setPersonaResponsableId(
															organigramaHijoD.getPersonaResponsableId());
													hijoD.setNombres(organigramaHijoD.getNombres());
													hijoD.setApellidoPaterno(organigramaHijoD.getApellidoPaterno());
													hijoD.setApellidoMaterno(organigramaHijoD.getApellidoMaterno());
													hijoD.setTipoDocumentoId(organigramaHijoD.getTipoDocumentoId());
													hijoD.setTipoDocumento(organigramaHijoD.getTipoDocumento());
													hijoD.setNumeroDocumento(organigramaHijoD.getNumeroDocumento());
													hijoD.setTelefonoId(organigramaHijoD.getTelefonoId());
													hijoD.setTelefono(organigramaHijoD.getTelefono());
													hijoD.setCorreoId(organigramaHijoD.getCorreoId());
													hijoD.setCorreo(organigramaHijoD.getCorreo());
													hijoD.setPaisId(organigramaHijoD.getPaisId());
													hijoD.setNombrePais(organigramaHijoD.getNombrePais());
													hijoD.setPadreId(organigramaHijoD.getPadreId());
													List<RespListaOrganigrama.OrganigramaHijoE> listOrganigramaHijoE = new ArrayList<>();

													Collections.sort(listaOrganigramaHijoE,
															new Comparator<ListarOrganigramaDTO>() {// NOSONAR
																@Override
																public int compare(ListarOrganigramaDTO p1,
																		ListarOrganigramaDTO p2) {
																	return p1.getIdOrganigrama()
																			.compareTo(p2.getIdOrganigrama());
																}
															});

													for (ListarOrganigramaDTO organigramaHijoE : listaOrganigramaHijoE) {
														RespListaOrganigrama.OrganigramaHijoE hijoE;
														if (organigramaHijoE.getPadreIdOrgHijo().toString().equals(
																organigramaHijoD.getIdOrganigrama().toString())) {

															hijoE = new RespListaOrganigrama.OrganigramaHijoE();
															hijoE.setIdOrganigrama(organigramaHijoD.getIdOrganigrama());
															hijoE.setIdEntidad(organigramaHijoD.getIdEntidad());
															hijoE.setAreaId(organigramaHijoD.getAreaId());
															hijoE.setSedeId(organigramaHijoD.getSedeId());
															hijoE.setPuesto(organigramaHijoD.getPuesto());
															hijoE.setOrden(organigramaHijoD.getOrden());
															hijoE.setDescripcion(organigramaHijoD.getDescripcion());
															hijoE.setNivelId(organigramaHijoD.getNivelId());
															hijoE.setDesNivel(organigramaHijoD.getDesNivel());
															hijoE.setSigla(organigramaHijoD.getSigla());
															hijoE.setNaturalezaId(organigramaHijoD.getNaturalezaId());
															hijoE.setDesNaturaleza(organigramaHijoD.getDesNaturaleza());
															hijoE.setEstadoId(organigramaHijoD.getEstadoId());
															hijoE.setEstado(organigramaHijoD.getEstado());
															hijoE.setPadreIdHijo(organigramaHijoD.getPadreIdOrgHijo());
															hijoE.setTipoOrganoId(organigramaHijoD.getTipoOrganoId());
															hijoE.setDesTipoOrgano(organigramaHijoD.getDesTipoOrgano());
															hijoE.setNivelGobiernoId(
																	organigramaHijoD.getNivelGobiernoId());
															hijoE.setDescripcionCorta(
																	organigramaHijoD.getDescripcionCorta());
															hijoE.setPersonaResponsableId(
																	organigramaHijoD.getPersonaResponsableId());
															hijoE.setNombres(organigramaHijoD.getNombres());
															hijoE.setApellidoPaterno(
																	organigramaHijoD.getApellidoPaterno());
															hijoE.setApellidoMaterno(
																	organigramaHijoD.getApellidoMaterno());
															hijoE.setTipoDocumentoId(
																	organigramaHijoD.getTipoDocumentoId());
															hijoE.setTipoDocumento(organigramaHijoD.getTipoDocumento());
															hijoE.setNumeroDocumento(
																	organigramaHijoD.getNumeroDocumento());
															hijoE.setTelefonoId(organigramaHijoD.getTelefonoId());
															hijoE.setTelefono(organigramaHijoD.getTelefono());
															hijoE.setCorreoId(organigramaHijoD.getCorreoId());
															hijoE.setCorreo(organigramaHijoD.getCorreo());
															hijoE.setPaisId(organigramaHijoD.getPaisId());
															hijoE.setNombrePais(organigramaHijoD.getNombrePais());
															hijoE.setPadreId(organigramaHijoD.getPadreId());
															List<RespListaOrganigrama.OrganigramaHijoF> listOrganigramaHijoF = new ArrayList<>();

															Collections.sort(listaOrganigramaHijoF,
																	new Comparator<ListarOrganigramaDTO>() {// NOSONAR
																		@Override
																		public int compare(ListarOrganigramaDTO p1,
																				ListarOrganigramaDTO p2) {
																			return p1.getIdOrganigrama()
																					.compareTo(p2.getIdOrganigrama());
																		}
																	});

															for (ListarOrganigramaDTO organigramaHijoF : listaOrganigramaHijoF) {
																RespListaOrganigrama.OrganigramaHijoF hijoF;
																if (organigramaHijoF.getPadreIdOrgHijo().toString()

																		.equals(organigramaHijoE.getIdOrganigrama()
																				.toString())) {
																	hijoF = new RespListaOrganigrama.OrganigramaHijoF();
																	hijoF.setIdOrganigrama(
																			organigramaHijoF.getIdOrganigrama());
																	hijoF.setIdEntidad(organigramaHijoF.getIdEntidad());
																	hijoF.setAreaId(organigramaHijoF.getAreaId());
																	hijoF.setSedeId(organigramaHijoF.getSedeId());
																	hijoF.setPuesto(organigramaHijoF.getPuesto());
																	hijoF.setOrden(organigramaHijoF.getOrden());
																	hijoF.setDescripcion(
																			organigramaHijoF.getDescripcion());
																	hijoF.setNivelId(organigramaHijoF.getNivelId());
																	hijoF.setDesNivel(organigramaHijoF.getDesNivel());
																	hijoF.setSigla(organigramaHijoF.getSigla());
																	hijoF.setNaturalezaId(
																			organigramaHijoF.getNaturalezaId());
			                                                     		
																	hijoF.setDesNaturaleza(
																			organigramaHijoF.getDesNaturaleza());
																	hijoF.setEstadoId(organigramaHijoF.getEstadoId());
																	hijoF.setEstado(organigramaHijoF.getEstado());
																	hijoF.setPadreIdHijo(
																			organigramaHijoF.getPadreIdOrgHijo());
																	hijoF.setTipoOrganoId(
																			organigramaHijoF.getTipoOrganoId());
																	hijoF.setDesTipoOrgano(
																			organigramaHijoF.getDesTipoOrgano());
																	hijoF.setNivelGobiernoId(
																			organigramaHijoF.getNivelGobiernoId());
																	hijoF.setDescripcionCorta(
																			organigramaHijoF.getDescripcionCorta());
																	hijoF.setPersonaResponsableId(
																			organigramaHijoF.getPersonaResponsableId());
																	hijoF.setNombres(organigramaHijoF.getNombres());
																	hijoF.setApellidoPaterno(
																			organigramaHijoF.getApellidoPaterno());
																	hijoF.setApellidoMaterno(
																			organigramaHijoF.getApellidoMaterno());
																	hijoF.setTipoDocumentoId(
																			organigramaHijoF.getTipoDocumentoId());
																	hijoF.setTipoDocumento(
																			organigramaHijoF.getTipoDocumento());
																	hijoF.setNumeroDocumento(
																			organigramaHijoF.getNumeroDocumento());
																	hijoF.setTelefonoId(
																			organigramaHijoF.getTelefonoId());
																	hijoF.setTelefono(organigramaHijoF.getTelefono());
																	hijoF.setCorreoId(organigramaHijoF.getCorreoId());
																	hijoF.setCorreo(organigramaHijoF.getCorreo());
																	hijoF.setPaisId(organigramaHijoF.getPaisId());
																	hijoF.setNombrePais(
																			organigramaHijoF.getNombrePais());
																	hijoF.setPadreId(organigramaHijoF.getPadreId());
																	listOrganigramaHijoF.add(hijoF);
																}
																hijoE.setListaOrganigramaHijoF(listOrganigramaHijoF);
															}
															listOrganigramaHijoE.add(hijoE);
														}
														hijoD.setListaOrganigramaHijoE(listOrganigramaHijoE);
													}
													listOrganigramaHijoD.add(hijoD);
												}
												hijoC.setListaOrganigramaHijoD(listOrganigramaHijoD);
											}
											listOrganigramaHijoC.add(hijoC);
										}
										hijoB.setListaOrganigramaHijoC(listOrganigramaHijoC);
									}
									listOrganigramaHijoB.add(hijoB);
								} // enf if B
								hijoA.setListaOrganigramaHijoB(listOrganigramaHijoB);
							}
							listOrganigramaHijoA.add(hijoA);
						} // end if A
						hijo.setListaOrganigramaHijoA(listOrganigramaHijoA);
					}
					listOrganigramaHijo.add(hijo);
				}
				organigrama.setListaOrganigramaHijo(listOrganigramaHijo);
			}
			responseOrganigramas.add(organigrama);
		}

	}

	private void generarListaOrganos(List<ListarOrganigramaDTOTest> listaOrganigrama,
			List<ListarOrganigramaDTOTest> listaOrganigramaHijo, // NOSONAR
			List<ListarOrganigramaDTOTest> listaOrganigramaHijoA, List<ListarOrganigramaDTOTest> listaOrganigramaHijoB,
			List<ListarOrganigramaDTOTest> listaOrganigramaHijoC, List<ListarOrganigramaDTOTest> listaOrganigramaHijoD,
			List<ListarOrganigramaDTOTest> listaOrganigramaHijoE, List<ListarOrganigramaDTOTest> listaOrganigramaHijoF,
			List<RespListaOrgano.OrganigramaPadre> responseOrganigramas) {
		RespListaOrgano.OrganigramaPadre organigrama;
		if (listaOrganigrama.isEmpty()) {
			Collections.sort(listaOrganigramaHijo, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
				@Override
				public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
					return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
				}
			});
			for (int i = listaOrganigramaHijo.size() - 1; i >= 0; i--) {
				if (listaOrganigramaHijo.size() > 1) {
					listaOrganigramaHijo.remove(i);
				}
			}

			for (ListarOrganigramaDTOTest organigramaHijo : listaOrganigramaHijo) {
				organigrama = new RespListaOrgano.OrganigramaPadre();
				organigrama.setOrganigramaId(organigramaHijo.getOrganigramaId());
				organigrama.setEntidadId(organigramaHijo.getEntidadId());
				organigrama.setAreaId(organigramaHijo.getAreaId());
				organigrama.setSedeId(organigramaHijo.getSedeId());
				organigrama.setPuesto(organigramaHijo.getPuesto());
				organigrama.setPuestoId(organigramaHijo.getPuestoId());
				organigrama.setUrlFoto(organigramaHijo.getUrlFoto());
				organigrama.setPuesto(organigramaHijo.getPuesto());
				organigrama.setOrden(organigramaHijo.getOrden());
				organigrama.setDescripcion(organigramaHijo.getDescripcion());
				organigrama.setNivelId(organigramaHijo.getNivelId());
				organigrama.setDesNivel(organigramaHijo.getDesNivel());
				organigrama.setSigla(organigramaHijo.getSigla());
				organigrama.setNaturalezaId(organigramaHijo.getNaturalezaId());
				organigrama.setDesNaturaleza(organigramaHijo.getDesNaturaleza());
				organigrama.setEstadoId(organigramaHijo.getEstadoId());
				organigrama.setEstado(organigramaHijo.getEstado());
				organigrama.setPadreIdHijo(organigramaHijo.getPadreIdOrgHijo());
				organigrama.setTipoOrganoId(organigramaHijo.getTipoOrganoId());
				organigrama.setDesTipoOrgano(organigramaHijo.getDesTipoOrgano());
				organigrama.setNivelGobiernoId(organigramaHijo.getNivelGobiernoId());
				organigrama.setDescripcionCorta(organigramaHijo.getDescripcionCorta());
				organigrama.setPersonaResponsableId(organigramaHijo.getPersonaResponsableId());
				organigrama.setNombres(organigramaHijo.getNombres());
				organigrama.setApellidoPaterno(organigramaHijo.getApellidoPaterno());
				organigrama.setApellidoMaterno(organigramaHijo.getApellidoMaterno());
				organigrama.setTipoDocumentoId(organigramaHijo.getTipoDocumentoId());
				organigrama.setTipoDocumento(organigramaHijo.getTipoDocumento());
				organigrama.setNumeroDocumento(organigramaHijo.getNumeroDocumento());
				organigrama.setTelefonoId(organigramaHijo.getTelefonoId());
				organigrama.setTelefono(organigramaHijo.getTelefono());
				organigrama.setCorreoId(organigramaHijo.getCorreoId());
				organigrama.setCorreo(organigramaHijo.getCorreo());
				organigrama.setPaisId(organigramaHijo.getPaisId());
				organigrama.setNombrePais(organigramaHijo.getNombrePais());
				organigrama.setPadreId(organigramaHijo.getPadreId());
				List<RespListaOrgano.OrganigramaHijo> listOrganigramaHijoA = new ArrayList<>();
				Collections.sort(listaOrganigramaHijoA, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
					@Override
					public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
						return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
					}
				});
				for (ListarOrganigramaDTOTest organigramaHijoA : listaOrganigramaHijoA) {
					RespListaOrgano.OrganigramaHijo hijoA;
					if (organigramaHijo.getOrganigramaId().toString()
							.equals(organigramaHijoA.getPadreIdOrgHijo().toString())) {
						hijoA = new RespListaOrgano.OrganigramaHijo();
						hijoA.setOrganigramaId(organigramaHijoA.getOrganigramaId());
						hijoA.setEntidadId(organigramaHijoA.getEntidadId());
						hijoA.setAreaId(organigramaHijoA.getAreaId());
						hijoA.setSedeId(organigramaHijoA.getSedeId());
						hijoA.setPuesto(organigramaHijoA.getPuesto());
						hijoA.setPuestoId(organigramaHijoA.getPuestoId());
						hijoA.setUrlFoto(organigramaHijoA.getUrlFoto());
						hijoA.setOrden(organigramaHijoA.getOrden());
						hijoA.setDescripcion(organigramaHijoA.getDescripcion());
						hijoA.setNivelId(organigramaHijoA.getNivelId());
						hijoA.setDesNivel(organigramaHijoA.getDesNivel());
						hijoA.setSigla(organigramaHijoA.getSigla());
						hijoA.setNaturalezaId(organigramaHijoA.getNaturalezaId());
						hijoA.setDesNaturaleza(organigramaHijoA.getDesNaturaleza());
						hijoA.setEstadoId(organigramaHijoA.getEstadoId());
						hijoA.setEstado(organigramaHijoA.getEstado());
						hijoA.setPadreIdHijo(organigramaHijoA.getPadreIdOrgHijo());
						hijoA.setTipoOrganoId(organigramaHijoA.getTipoOrganoId());
						hijoA.setDesTipoOrgano(organigramaHijoA.getDesTipoOrgano());
						hijoA.setNivelGobiernoId(organigramaHijoA.getNivelGobiernoId());
						hijoA.setDescripcionCorta(organigramaHijoA.getDescripcionCorta());
						hijoA.setPersonaResponsableId(organigramaHijoA.getPersonaResponsableId());
						hijoA.setNombres(organigramaHijoA.getNombres());
						hijoA.setApellidoPaterno(organigramaHijoA.getApellidoPaterno());
						hijoA.setApellidoMaterno(organigramaHijoA.getApellidoMaterno());
						hijoA.setTipoDocumentoId(organigramaHijoA.getTipoDocumentoId());
						hijoA.setTipoDocumento(organigramaHijoA.getTipoDocumento());
						hijoA.setNumeroDocumento(organigramaHijoA.getNumeroDocumento());
						hijoA.setTelefonoId(organigramaHijoA.getTelefonoId());
						hijoA.setTelefono(organigramaHijoA.getTelefono());
						hijoA.setCorreoId(organigramaHijoA.getCorreoId());
						hijoA.setCorreo(organigramaHijoA.getCorreo());
						hijoA.setPaisId(organigramaHijoA.getPaisId());
						hijoA.setNombrePais(organigramaHijoA.getNombrePais());
						hijoA.setPadreId(organigramaHijoA.getPadreId());
						List<RespListaOrgano.OrganigramaHijoA> listOrganigramaHijoB = new ArrayList<>();
						Collections.sort(listaOrganigramaHijoB, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
							@Override
							public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
								return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
							}
						});
						for (ListarOrganigramaDTOTest organigramaHijoB : listaOrganigramaHijoB) {
							RespListaOrgano.OrganigramaHijoA hijoB = null;
							if (organigramaHijoB.getPadreIdOrgHijo().toString()
									.equals(organigramaHijoA.getOrganigramaId().toString())) {
								hijoB = new RespListaOrgano.OrganigramaHijoA();
								hijoB.setOrganigramaId(organigramaHijoB.getOrganigramaId());
								hijoB.setEntidadId(organigramaHijoB.getEntidadId());
								hijoB.setAreaId(organigramaHijoB.getAreaId());
								hijoB.setSedeId(organigramaHijoB.getSedeId());
								hijoB.setPuesto(organigramaHijoB.getPuesto());
								hijoB.setPuestoId(organigramaHijoB.getPuestoId());
								hijoB.setUrlFoto(organigramaHijoB.getUrlFoto());
								hijoB.setOrden(organigramaHijoB.getOrden());
								hijoB.setDescripcion(organigramaHijoB.getDescripcion());
								hijoB.setNivelId(organigramaHijoB.getNivelId());
								hijoB.setDesNivel(organigramaHijoB.getDesNivel());
								hijoB.setSigla(organigramaHijoB.getSigla());
								hijoB.setNaturalezaId(organigramaHijoB.getNaturalezaId());
								hijoB.setDesNaturaleza(organigramaHijoB.getDesNaturaleza());
								hijoB.setEstadoId(organigramaHijoB.getEstadoId());
								hijoB.setEstado(organigramaHijoB.getEstado());
								hijoB.setPadreIdHijo(organigramaHijoB.getPadreIdOrgHijo());
								hijoB.setTipoOrganoId(organigramaHijoB.getTipoOrganoId());
								hijoB.setDesTipoOrgano(organigramaHijoB.getDesTipoOrgano());
								hijoB.setNivelGobiernoId(organigramaHijoB.getNivelGobiernoId());
								hijoB.setDescripcionCorta(organigramaHijoB.getDescripcionCorta());
								hijoB.setPersonaResponsableId(organigramaHijoB.getPersonaResponsableId());
								hijoB.setNombres(organigramaHijoB.getNombres());
								hijoB.setApellidoPaterno(organigramaHijoB.getApellidoPaterno());
								hijoB.setApellidoMaterno(organigramaHijoB.getApellidoMaterno());
								hijoB.setTipoDocumentoId(organigramaHijoB.getTipoDocumentoId());
								hijoB.setTipoDocumento(organigramaHijoB.getTipoDocumento());
								hijoB.setNumeroDocumento(organigramaHijoB.getNumeroDocumento());
								hijoB.setTelefonoId(organigramaHijoB.getTelefonoId());
								hijoB.setTelefono(organigramaHijoB.getTelefono());
								hijoB.setCorreoId(organigramaHijoB.getCorreoId());
								hijoB.setCorreo(organigramaHijoB.getCorreo());
								hijoB.setPaisId(organigramaHijoB.getPaisId());
								hijoB.setNombrePais(organigramaHijoB.getNombrePais());
								hijoB.setPadreId(organigramaHijoB.getPadreId());
								listOrganigramaHijoB.add(hijoB);
							}
							hijoA.setListaOrganigramaHijoA(listOrganigramaHijoB);
						}
						listOrganigramaHijoA.add(hijoA);
					}
					organigrama.setListaOrganigramaHijo(listOrganigramaHijoA);
				}
				responseOrganigramas.add(organigrama);
			}
		}
		for (ListarOrganigramaDTOTest organigramaPadre : listaOrganigrama) {
			organigrama = new RespListaOrgano.OrganigramaPadre();
			organigrama.setOrganigramaId(organigramaPadre.getOrganigramaId());
			organigrama.setEntidadId(organigramaPadre.getEntidadId());
			organigrama.setAreaId(organigramaPadre.getAreaId());
			organigrama.setSedeId(organigramaPadre.getSedeId());
			organigrama.setPuesto(organigramaPadre.getPuesto());
			organigrama.setPuestoId(organigramaPadre.getPuestoId());
			organigrama.setUrlFoto(organigramaPadre.getUrlFoto());
			organigrama.setOrden(organigramaPadre.getOrden());
			organigrama.setDescripcion(organigramaPadre.getDescripcion());
			organigrama.setNivelId(organigramaPadre.getNivelId());
			organigrama.setDesNivel(organigramaPadre.getDesNivel());
			organigrama.setSigla(organigramaPadre.getSigla());
			organigrama.setNaturalezaId(organigramaPadre.getNaturalezaId());
			organigrama.setDesNaturaleza(organigramaPadre.getDesNaturaleza());
			organigrama.setEstadoId(organigramaPadre.getEstadoId());
			organigrama.setEstado(organigramaPadre.getEstado());
			organigrama.setPadreIdHijo(organigramaPadre.getPadreIdOrgHijo());
			organigrama.setTipoOrganoId(organigramaPadre.getTipoOrganoId());
			organigrama.setDesTipoOrgano(organigramaPadre.getDesTipoOrgano());
			organigrama.setNivelGobiernoId(organigramaPadre.getNivelGobiernoId());
			organigrama.setDescripcionCorta(organigramaPadre.getDescripcionCorta());
			organigrama.setPersonaResponsableId(organigramaPadre.getPersonaResponsableId());
			organigrama.setNombres(organigramaPadre.getNombres());
			organigrama.setApellidoPaterno(organigramaPadre.getApellidoPaterno());
			organigrama.setApellidoMaterno(organigramaPadre.getApellidoMaterno());
			organigrama.setTipoDocumentoId(organigramaPadre.getTipoDocumentoId());
			organigrama.setTipoDocumento(organigramaPadre.getTipoDocumento());
			organigrama.setNumeroDocumento(organigramaPadre.getNumeroDocumento());
			organigrama.setTelefonoId(organigramaPadre.getTelefonoId());
			organigrama.setTelefono(organigramaPadre.getTelefono());
			organigrama.setCorreoId(organigramaPadre.getCorreoId());
			organigrama.setCorreo(organigramaPadre.getCorreo());
			organigrama.setPaisId(organigramaPadre.getPaisId());
			organigrama.setNombrePais(organigramaPadre.getNombrePais());
			organigrama.setPadreId(organigramaPadre.getPadreId());
			List<RespListaOrgano.OrganigramaHijo> listOrganigramaHijo = new ArrayList<>();
			Collections.sort(listaOrganigramaHijo, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
				@Override
				public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
					return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
				}
			});
			for (ListarOrganigramaDTOTest organigramaHijo : listaOrganigramaHijo) {
				RespListaOrgano.OrganigramaHijo hijo;
				if (organigramaPadre.getOrganigramaId().toString()
						.equals(organigramaHijo.getPadreIdOrgHijo().toString())) {
					hijo = new RespListaOrgano.OrganigramaHijo();
					hijo.setOrganigramaId(organigramaHijo.getOrganigramaId());
					hijo.setEntidadId(organigramaHijo.getEntidadId());
					hijo.setAreaId(organigramaHijo.getAreaId());
					hijo.setSedeId(organigramaHijo.getSedeId());
					hijo.setPuesto(organigramaHijo.getPuesto());
					hijo.setPuestoId(organigramaHijo.getPuestoId());
					hijo.setUrlFoto(organigramaHijo.getUrlFoto());
					hijo.setOrden(organigramaHijo.getOrden());
					hijo.setDescripcion(organigramaHijo.getDescripcion());
					hijo.setNivelId(organigramaHijo.getNivelId());
					hijo.setDesNivel(organigramaHijo.getDesNivel());
					hijo.setSigla(organigramaHijo.getSigla());			
					hijo.setNaturalezaId(organigramaHijo.getNaturalezaId());
					hijo.setDesNaturaleza(organigramaHijo.getDesNaturaleza());
					hijo.setEstadoId(organigramaHijo.getEstadoId());
					hijo.setEstado(organigramaHijo.getEstado());
					hijo.setPadreIdHijo(organigramaHijo.getPadreIdOrgHijo());
					hijo.setTipoOrganoId(organigramaHijo.getTipoOrganoId());
					hijo.setDesTipoOrgano(organigramaHijo.getDesTipoOrgano());
					hijo.setNivelGobiernoId(organigramaHijo.getNivelGobiernoId());
					hijo.setDescripcionCorta(organigramaHijo.getDescripcionCorta());
					hijo.setPersonaResponsableId(organigramaHijo.getPersonaResponsableId());
					hijo.setNombres(organigramaHijo.getNombres());
					hijo.setApellidoPaterno(organigramaHijo.getApellidoPaterno());
					hijo.setApellidoMaterno(organigramaHijo.getApellidoMaterno());
					hijo.setTipoDocumentoId(organigramaHijo.getTipoDocumentoId());
					hijo.setTipoDocumento(organigramaHijo.getTipoDocumento());
					hijo.setNumeroDocumento(organigramaHijo.getNumeroDocumento());
					hijo.setTelefonoId(organigramaHijo.getTelefonoId());
					hijo.setTelefono(organigramaHijo.getTelefono());
					hijo.setCorreoId(organigramaHijo.getCorreoId());
					hijo.setCorreo(organigramaHijo.getCorreo());
					hijo.setPaisId(organigramaHijo.getPaisId());
					hijo.setNombrePais(organigramaHijo.getNombrePais());
					hijo.setPadreId(organigramaHijo.getPadreId());
					List<RespListaOrgano.OrganigramaHijoA> listOrganigramaHijoA = new ArrayList<>();
					Collections.sort(listaOrganigramaHijoA, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
						@Override
						public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
							return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
						}
					});
					for (ListarOrganigramaDTOTest organigramaHijoA : listaOrganigramaHijoA) {
						RespListaOrgano.OrganigramaHijoA hijoA = null;
						if (organigramaHijoA.getPadreIdOrgHijo().toString()
								.equals(organigramaHijo.getOrganigramaId().toString())) {
							hijoA = new RespListaOrgano.OrganigramaHijoA();
							hijoA.setOrganigramaId(organigramaHijoA.getOrganigramaId());
							hijoA.setEntidadId(organigramaHijoA.getEntidadId());
							hijoA.setAreaId(organigramaHijoA.getAreaId());
							hijoA.setSedeId(organigramaHijoA.getSedeId());
							hijoA.setPuesto(organigramaHijoA.getPuesto());
							hijoA.setPuestoId(organigramaHijoA.getPuestoId());
							hijoA.setUrlFoto(organigramaHijoA.getUrlFoto());
							hijoA.setOrden(organigramaHijoA.getOrden());
							hijoA.setDescripcion(organigramaHijoA.getDescripcion());
							hijoA.setNivelId(organigramaHijoA.getNivelId());
							hijoA.setDesNivel(organigramaHijoA.getDesNivel());
							hijoA.setSigla(organigramaHijoA.getSigla());				
							hijoA.setNaturalezaId(organigramaHijoA.getNaturalezaId());
							hijoA.setDesNaturaleza(organigramaHijoA.getDesNaturaleza());
							hijoA.setEstadoId(organigramaHijoA.getEstadoId());
							hijoA.setEstado(organigramaHijoA.getEstado());
							hijoA.setPadreIdHijo(organigramaHijoA.getPadreIdOrgHijo());
							hijoA.setTipoOrganoId(organigramaHijoA.getTipoOrganoId());
							hijoA.setDesTipoOrgano(organigramaHijoA.getDesTipoOrgano());
							hijoA.setNivelGobiernoId(organigramaHijoA.getNivelGobiernoId());
							hijoA.setDescripcionCorta(organigramaHijoA.getDescripcionCorta());
							hijoA.setPersonaResponsableId(organigramaHijoA.getPersonaResponsableId());
							hijoA.setNombres(organigramaHijoA.getNombres());
							hijoA.setApellidoPaterno(organigramaHijoA.getApellidoPaterno());
							hijoA.setApellidoMaterno(organigramaHijoA.getApellidoMaterno());
							hijoA.setTipoDocumentoId(organigramaHijoA.getTipoDocumentoId());
							hijoA.setTipoDocumento(organigramaHijoA.getTipoDocumento());
							hijoA.setNumeroDocumento(organigramaHijoA.getNumeroDocumento());
							hijoA.setTelefonoId(organigramaHijoA.getTelefonoId());
							hijoA.setTelefono(organigramaHijoA.getTelefono());
							hijoA.setCorreoId(organigramaHijoA.getCorreoId());
							hijoA.setCorreo(organigramaHijoA.getCorreo());
							hijoA.setPaisId(organigramaHijoA.getPaisId());
							hijoA.setNombrePais(organigramaHijoA.getNombrePais());
							hijoA.setPadreId(organigramaHijoA.getPadreId());

							List<RespListaOrgano.OrganigramaHijoB> listOrganigramaHijoB = new ArrayList<>();
							Collections.sort(listaOrganigramaHijoB, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
								@Override
								public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
									return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
								}
							});
							for (ListarOrganigramaDTOTest organigramaHijoB : listaOrganigramaHijoB) {
								RespListaOrgano.OrganigramaHijoB hijoB = null;
								if (organigramaHijoB.getPadreIdOrgHijo().toString()
										.equals(organigramaHijoA.getOrganigramaId().toString())) {
									hijoB = new RespListaOrgano.OrganigramaHijoB();
									hijoB.setOrganigramaId(organigramaHijoB.getOrganigramaId());
									hijoB.setEntidadId(organigramaHijoB.getEntidadId());
									hijoB.setAreaId(organigramaHijoB.getAreaId());
									hijoB.setSedeId(organigramaHijoB.getSedeId());
									hijoB.setPuesto(organigramaHijoB.getPuesto());
									hijoB.setPuestoId(organigramaHijoB.getPuestoId());
									hijoB.setUrlFoto(organigramaHijoB.getUrlFoto());
									hijoB.setOrden(organigramaHijoB.getOrden());
									hijoB.setDescripcion(organigramaHijoB.getDescripcion());
									hijoB.setNivelId(organigramaHijoB.getNivelId());
									hijoB.setDesNivel(organigramaHijoB.getDesNivel());
									hijoB.setSigla(organigramaHijoB.getSigla());				
									hijoB.setNaturalezaId(organigramaHijoB.getNaturalezaId());
									hijoB.setDesNaturaleza(organigramaHijoB.getDesNaturaleza());
									hijoB.setEstadoId(organigramaHijoB.getEstadoId());
									hijoB.setEstado(organigramaHijoB.getEstado());
									hijoB.setPadreIdHijo(organigramaHijoB.getPadreIdOrgHijo());
									hijoB.setTipoOrganoId(organigramaHijoB.getTipoOrganoId());
									hijoB.setDesTipoOrgano(organigramaHijoB.getDesTipoOrgano());
									hijoB.setNivelGobiernoId(organigramaHijoB.getNivelGobiernoId());
									hijoB.setDescripcionCorta(organigramaHijoB.getDescripcionCorta());
									hijoB.setPersonaResponsableId(organigramaHijoB.getPersonaResponsableId());
									hijoB.setNombres(organigramaHijoB.getNombres());
									hijoB.setApellidoPaterno(organigramaHijoB.getApellidoPaterno());
									hijoB.setApellidoMaterno(organigramaHijoB.getApellidoMaterno());
									hijoB.setTipoDocumentoId(organigramaHijoB.getTipoDocumentoId());
									hijoB.setTipoDocumento(organigramaHijoB.getTipoDocumento());
									hijoB.setNumeroDocumento(organigramaHijoB.getNumeroDocumento());
									hijoB.setTelefonoId(organigramaHijoB.getTelefonoId());
									hijoB.setTelefono(organigramaHijoB.getTelefono());
									hijoB.setCorreoId(organigramaHijoB.getCorreoId());
									hijoB.setCorreo(organigramaHijoB.getCorreo());
									hijoB.setPaisId(organigramaHijoB.getPaisId());
									hijoB.setNombrePais(organigramaHijoB.getNombrePais());
									hijoB.setPadreId(organigramaHijoB.getPadreId());

									List<RespListaOrgano.OrganigramaHijoC> listOrganigramaHijoC = new ArrayList<>();
									Collections.sort(listaOrganigramaHijoC, new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
										@Override
										public int compare(ListarOrganigramaDTOTest p1, ListarOrganigramaDTOTest p2) {
											return p1.getOrganigramaId().compareTo(p2.getOrganigramaId());
										}
									});

									for (ListarOrganigramaDTOTest organigramaHijoC : listaOrganigramaHijoC) {
										RespListaOrgano.OrganigramaHijoC hijoC;
										if (organigramaHijoC.getPadreIdOrgHijo().toString()
												.equals(organigramaHijoB.getOrganigramaId().toString())) {
											hijoC = new RespListaOrgano.OrganigramaHijoC();
											hijoC.setOrganigramaId(organigramaHijoC.getOrganigramaId());
											hijoC.setEntidadId(organigramaHijoC.getEntidadId());
											hijoC.setAreaId(organigramaHijoC.getAreaId());
											hijoC.setSedeId(organigramaHijoC.getSedeId());
											hijoC.setPuesto(organigramaHijoC.getPuesto());
											hijoC.setPuestoId(organigramaHijoC.getPuestoId());
											hijoC.setUrlFoto(organigramaHijoC.getUrlFoto());
											hijoC.setOrden(organigramaHijoC.getOrden());
											hijoC.setDescripcion(organigramaHijoC.getDescripcion());
											hijoC.setNivelId(organigramaHijoC.getNivelId());
											hijoC.setDesNivel(organigramaHijoC.getDesNivel());
											hijoC.setSigla(organigramaHijoC.getSigla());				
											hijoC.setNaturalezaId(organigramaHijoC.getNaturalezaId());
											hijoC.setDesNaturaleza(organigramaHijoC.getDesNaturaleza());
											hijoC.setEstadoId(organigramaHijoC.getEstadoId());
											hijoC.setEstado(organigramaHijoC.getEstado());
											hijoC.setPadreIdHijo(organigramaHijoC.getPadreIdOrgHijo());
											hijoC.setTipoOrganoId(organigramaHijoC.getTipoOrganoId());
											hijoC.setDesTipoOrgano(organigramaHijoC.getDesTipoOrgano());
											hijoC.setNivelGobiernoId(organigramaHijoC.getNivelGobiernoId());
											hijoC.setDescripcionCorta(organigramaHijoC.getDescripcionCorta());
											hijoC.setPersonaResponsableId(organigramaHijoC.getPersonaResponsableId());
											hijoC.setNombres(organigramaHijoC.getNombres());
											hijoC.setApellidoPaterno(organigramaHijoC.getApellidoPaterno());
											hijoC.setApellidoMaterno(organigramaHijoC.getApellidoMaterno());
											hijoC.setTipoDocumentoId(organigramaHijoC.getTipoDocumentoId());
											hijoC.setTipoDocumento(organigramaHijoC.getTipoDocumento());
											hijoC.setNumeroDocumento(organigramaHijoC.getNumeroDocumento());
											hijoC.setTelefonoId(organigramaHijoC.getTelefonoId());
											hijoC.setTelefono(organigramaHijoC.getTelefono());
											hijoC.setCorreoId(organigramaHijoC.getCorreoId());
											hijoC.setCorreo(organigramaHijoC.getCorreo());
											hijoC.setPaisId(organigramaHijoC.getPaisId());
											hijoC.setNombrePais(organigramaHijoC.getNombrePais());
											hijoC.setPadreId(organigramaHijoC.getPadreId());

											List<RespListaOrgano.OrganigramaHijoD> listOrganigramaHijoD = new ArrayList<>();
											Collections.sort(listaOrganigramaHijoD,
													new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
														@Override
														public int compare(ListarOrganigramaDTOTest p1,
																ListarOrganigramaDTOTest p2) {
															return p1.getOrganigramaId()
																	.compareTo(p2.getOrganigramaId());
														}
													});

											for (ListarOrganigramaDTOTest organigramaHijoD : listaOrganigramaHijoD) {
												RespListaOrgano.OrganigramaHijoD hijoD;
												if (organigramaHijoD.getPadreIdOrgHijo().toString()
														.equals(organigramaHijoC.getOrganigramaId().toString())) {
													hijoD = new RespListaOrgano.OrganigramaHijoD();
													hijoD.setOrganigramaId(organigramaHijoD.getOrganigramaId());
													hijoD.setEntidadId(organigramaHijoD.getEntidadId());
													hijoD.setAreaId(organigramaHijoD.getAreaId());
													hijoD.setSedeId(organigramaHijoD.getSedeId());
													hijoD.setPuesto(organigramaHijoD.getPuesto());
													hijoD.setPuestoId(organigramaHijoD.getPuestoId());
													hijoD.setUrlFoto(organigramaHijoD.getUrlFoto());
													hijoD.setOrden(organigramaHijoD.getOrden());
													hijoD.setDescripcion(organigramaHijoD.getDescripcion());
													hijoD.setNivelId(organigramaHijoD.getNivelId());
													hijoD.setDesNivel(organigramaHijoD.getDesNivel());
													hijoD.setSigla(organigramaHijoD.getSigla());				
													hijoD.setNaturalezaId(organigramaHijoD.getNaturalezaId());
													hijoD.setDesNaturaleza(organigramaHijoD.getDesNaturaleza());
													hijoD.setEstadoId(organigramaHijoD.getEstadoId());
													hijoD.setEstado(organigramaHijoD.getEstado());
													hijoD.setPadreIdHijo(organigramaHijoD.getPadreIdOrgHijo());
													hijoD.setTipoOrganoId(organigramaHijoD.getTipoOrganoId());
													hijoD.setDesTipoOrgano(organigramaHijoD.getDesTipoOrgano());
													hijoD.setNivelGobiernoId(organigramaHijoD.getNivelGobiernoId());
													hijoD.setDescripcionCorta(organigramaHijoD.getDescripcionCorta());
													hijoD.setPersonaResponsableId(
															organigramaHijoD.getPersonaResponsableId());
													hijoD.setNombres(organigramaHijoD.getNombres());
													hijoD.setApellidoPaterno(organigramaHijoD.getApellidoPaterno());
													hijoD.setApellidoMaterno(organigramaHijoD.getApellidoMaterno());
													hijoD.setTipoDocumentoId(organigramaHijoD.getTipoDocumentoId());
													hijoD.setTipoDocumento(organigramaHijoD.getTipoDocumento());
													hijoD.setNumeroDocumento(organigramaHijoD.getNumeroDocumento());
													hijoD.setTelefonoId(organigramaHijoD.getTelefonoId());
													hijoD.setTelefono(organigramaHijoD.getTelefono());
													hijoD.setCorreoId(organigramaHijoD.getCorreoId());
													hijoD.setCorreo(organigramaHijoD.getCorreo());
													hijoD.setPaisId(organigramaHijoD.getPaisId());
													hijoD.setNombrePais(organigramaHijoD.getNombrePais());
													hijoD.setPadreId(organigramaHijoD.getPadreId());
													List<RespListaOrgano.OrganigramaHijoE> listOrganigramaHijoE = new ArrayList<>();
													Collections.sort(listaOrganigramaHijoE,
															new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
																@Override
																public int compare(ListarOrganigramaDTOTest p1,
																		ListarOrganigramaDTOTest p2) {
																	return p1.getOrganigramaId()
																			.compareTo(p2.getOrganigramaId());
																}
															});

													for (ListarOrganigramaDTOTest organigramaHijoE : listaOrganigramaHijoE) {
														RespListaOrgano.OrganigramaHijoE hijoE;
														if (organigramaHijoE.getPadreIdOrgHijo().toString().equals(
																organigramaHijoD.getOrganigramaId().toString())) {
															hijoE = new RespListaOrgano.OrganigramaHijoE();
															hijoE.setOrganigramaId(organigramaHijoD.getOrganigramaId());
															hijoE.setEntidadId(organigramaHijoD.getEntidadId());
															hijoE.setAreaId(organigramaHijoD.getAreaId());
															hijoE.setSedeId(organigramaHijoD.getSedeId());
															hijoE.setPuesto(organigramaHijoD.getPuesto());
															hijoE.setPuestoId(organigramaHijoE.getPuestoId());
															hijoE.setUrlFoto(organigramaHijoE.getUrlFoto());
															hijoE.setOrden(organigramaHijoD.getOrden());
															hijoE.setDescripcion(organigramaHijoD.getDescripcion());
															hijoE.setNivelId(organigramaHijoD.getNivelId());
															hijoE.setDesNivel(organigramaHijoD.getDesNivel());
															hijoE.setSigla(organigramaHijoD.getSigla());				
															hijoE.setNaturalezaId(organigramaHijoD.getNaturalezaId());
															hijoE.setDesNaturaleza(organigramaHijoD.getDesNaturaleza());
															hijoE.setEstadoId(organigramaHijoD.getEstadoId());
															hijoE.setEstado(organigramaHijoD.getEstado());
															hijoE.setPadreIdHijo(organigramaHijoD.getPadreIdOrgHijo());
															hijoE.setTipoOrganoId(organigramaHijoD.getTipoOrganoId());
															hijoE.setDesTipoOrgano(organigramaHijoD.getDesTipoOrgano());
															hijoE.setNivelGobiernoId(
																	organigramaHijoD.getNivelGobiernoId());
															hijoE.setDescripcionCorta(
																	organigramaHijoD.getDescripcionCorta());
															hijoE.setPersonaResponsableId(
																	organigramaHijoD.getPersonaResponsableId());
															hijoE.setNombres(organigramaHijoD.getNombres());
															hijoE.setApellidoPaterno(
																	organigramaHijoD.getApellidoPaterno());
															hijoE.setApellidoMaterno(
																	organigramaHijoD.getApellidoMaterno());
															hijoE.setTipoDocumentoId(
																	organigramaHijoD.getTipoDocumentoId());
															hijoE.setTipoDocumento(organigramaHijoD.getTipoDocumento());
															hijoE.setNumeroDocumento(
																	organigramaHijoD.getNumeroDocumento());
															hijoE.setTelefonoId(organigramaHijoD.getTelefonoId());
															hijoE.setTelefono(organigramaHijoD.getTelefono());
															hijoE.setCorreoId(organigramaHijoD.getCorreoId());
															hijoE.setCorreo(organigramaHijoD.getCorreo());
															hijoE.setPaisId(organigramaHijoD.getPaisId());
															hijoE.setNombrePais(organigramaHijoD.getNombrePais());
															hijoE.setPadreId(organigramaHijoD.getPadreId());
															List<RespListaOrgano.OrganigramaHijoF> listOrganigramaHijoF = new ArrayList<>();
															Collections.sort(listaOrganigramaHijoF,
																	new Comparator<ListarOrganigramaDTOTest>() {// NOSONAR
																		@Override
																		public int compare(ListarOrganigramaDTOTest p1,
																				ListarOrganigramaDTOTest p2) {
																			return p1.getOrganigramaId()
																					.compareTo(p2.getOrganigramaId());
																		}
																	});

															for (ListarOrganigramaDTOTest organigramaHijoF : listaOrganigramaHijoF) {
																RespListaOrgano.OrganigramaHijoF hijoF;
																if (organigramaHijoF.getPadreIdOrgHijo().toString()
																		.equals(organigramaHijoE.getOrganigramaId()
																				.toString())) {
																	hijoF = new RespListaOrgano.OrganigramaHijoF();
																	hijoF.setOrganigramaId(
																			organigramaHijoF.getOrganigramaId());
																	hijoF.setEntidadId(organigramaHijoF.getEntidadId());

																	hijoF.setAreaId(organigramaHijoF.getAreaId());
																	hijoF.setSedeId(organigramaHijoF.getSedeId());
																	hijoF.setPuesto(organigramaHijoF.getPuesto());
																	hijoF.setPuestoId(organigramaHijoF.getPuestoId());
																	hijoF.setUrlFoto(organigramaHijoF.getUrlFoto());
																	hijoF.setOrden(organigramaHijoF.getOrden());
																	hijoF.setDescripcion(
																			organigramaHijoF.getDescripcion());
																	hijoF.setNivelId(organigramaHijoF.getNivelId());
																	hijoF.setDesNivel(organigramaHijoF.getDesNivel());
																	hijoF.setSigla(organigramaHijoF.getSigla());				
																	hijoF.setNaturalezaId(
																			organigramaHijoF.getNaturalezaId());
																	hijoF.setDesNaturaleza(
																			organigramaHijoF.getDesNaturaleza());
																	hijoF.setEstadoId(organigramaHijoF.getEstadoId());
																	hijoF.setEstado(organigramaHijoF.getEstado());
																	hijoF.setPadreIdHijo(
																			organigramaHijoF.getPadreIdOrgHijo());
																	hijoF.setTipoOrganoId(
																			organigramaHijoF.getTipoOrganoId());
																	hijoF.setDesTipoOrgano(
																			organigramaHijoF.getDesTipoOrgano());
																	hijoF.setNivelGobiernoId(
																			organigramaHijoF.getNivelGobiernoId());
																	hijoF.setDescripcionCorta(
																			organigramaHijoF.getDescripcionCorta());
																	hijoF.setPersonaResponsableId(
																			organigramaHijoF.getPersonaResponsableId());
																	hijoF.setNombres(organigramaHijoF.getNombres());
																	hijoF.setApellidoPaterno(
																			organigramaHijoF.getApellidoPaterno());
																	hijoF.setApellidoMaterno(
																			organigramaHijoF.getApellidoMaterno());
																	hijoF.setTipoDocumentoId(
																			organigramaHijoF.getTipoDocumentoId());
																	hijoF.setTipoDocumento(
																			organigramaHijoF.getTipoDocumento());
																	hijoF.setNumeroDocumento(
																			organigramaHijoF.getNumeroDocumento());
																	hijoF.setTelefonoId(
																			organigramaHijoF.getTelefonoId());
																	hijoF.setTelefono(organigramaHijoF.getTelefono());
																	hijoF.setCorreoId(organigramaHijoF.getCorreoId());
																	hijoF.setCorreo(organigramaHijoF.getCorreo());
																	hijoF.setPaisId(organigramaHijoF.getPaisId());
																	hijoF.setNombrePais(
																			organigramaHijoF.getNombrePais());
																	hijoF.setPadreId(organigramaHijoF.getPadreId());
																	listOrganigramaHijoF.add(hijoF);
																}
																hijoE.setListaOrganigramaHijoF(listOrganigramaHijoF);
																hijoE.setCantidadHijoF(listOrganigramaHijoF.size());
															}
															listOrganigramaHijoE.add(hijoE);
														}
														hijoD.setListaOrganigramaHijoE(listOrganigramaHijoE);
														hijoD.setCantidadHijoE(listOrganigramaHijoE.size());
													}
													listOrganigramaHijoD.add(hijoD);
												}
												hijoC.setListaOrganigramaHijoD(listOrganigramaHijoD);
												hijoC.setCantidadHijoD(listOrganigramaHijoD.size());
											}
											listOrganigramaHijoC.add(hijoC);
										}
										hijoB.setListaOrganigramaHijoC(listOrganigramaHijoC);
										hijoB.setCantidadHijoC(listOrganigramaHijoC.size());
									}
									listOrganigramaHijoB.add(hijoB);
								} // enf if B
								hijoA.setListaOrganigramaHijoB(listOrganigramaHijoB);
								hijoA.setCantidadHijoB(listOrganigramaHijoB.size());
							}
							listOrganigramaHijoA.add(hijoA);
						} // end if A
						hijo.setListaOrganigramaHijoA(listOrganigramaHijoA);
						hijo.setCantidadHijoA(listOrganigramaHijoA.size());
					}
					listOrganigramaHijo.add(hijo);
				}
				organigrama.setListaOrganigramaHijo(listOrganigramaHijo);
			}
			responseOrganigramas.add(organigrama);
		}

	}

	@Override
	public RespBase<RespComboPerByOrganigrama> comboPersonaByOrganigrama(Map<String, Object> parametroMap) {
		RespComboPerByOrganigrama respPayload = new RespComboPerByOrganigrama();
		List<ComboByOrganigrama> comboOrganigrama = gestionRepository.comboPersonaByOrganigrama(parametroMap);
		respPayload.setComboPersonas(comboOrganigrama);
		return new RespBase<RespComboPerByOrganigrama>().ok(respPayload);
	}

	@Override
	public RespBase<RespObtenerOrganigrama> buscarOrganigramas() {
		Map<String, Object> parametroMap = new HashMap<>();
		List<OrganigramaDTO> lista = gestionRepository.buscarOrganigramaByFilter(parametroMap);
		RespObtenerOrganigrama respPayload = new RespObtenerOrganigrama();
		respPayload.setListaOrganigrama(lista);
		return new RespBase<RespObtenerOrganigrama>().ok(respPayload);
	}

	@Override
	public List<OrganoExcelDTO> validarCargaMasivaOrganigrama(InputStream uploadedInputStream) {
		try {
			ExcelUtil<OrganoExcelDTO> file = new ExcelUtil<>(OrganoExcelDTO::new);
			List<OrganoExcelDTO> listaOrganigrama = file.utilExcelToPojo(uploadedInputStream,
					Constantes.HOJA_EXCEL_CERO);// NOSONAR
			return listaOrganigrama;

		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);

		}
		return null;// NOSONAR

	}

	@Override
	public List<OrganigramaExcelDTO> validarCargaMasivaOrgExcel(InputStream uploadedInputStream) {
		List<OrganigramaExcelDTO> listaOrgano = new ArrayList<>();
		try {
			XSSFWorkbook workbook = new XSSFWorkbook(uploadedInputStream);
			XSSFSheet hojaDatos = workbook.getSheet("DATOS");
			int filas = hojaDatos.getLastRowNum();
			int columnas = 4;

			for (int f = 1; f <= filas; f++) {
				XSSFRow filaInicial = hojaDatos.getRow(f);
				OrganigramaExcelDTO datos = new OrganigramaExcelDTO();
				for (int c = 0; c < columnas; c++) {
					XSSFCell celda = filaInicial.getCell(c);
					if (celda == null) {
						celda = filaInicial.createCell(c);
						obtenerDatos(datos, c, celda);
					} else {
						obtenerDatos(datos, c, celda);
					}
				}
				listaOrgano.add(datos);
			}
			workbook.close();
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);

		}
		return listaOrgano;// NOSONAR

	}

	private void obtenerDatos(OrganigramaExcelDTO datos, int c, XSSFCell celda) {

		if (celda.getColumnIndex() == 0 && (celda.getCellTypeEnum() == CellType.STRING
				|| celda.getCellTypeEnum() == CellType.BLANK)) {
			datos.setTipoOrgano(celda.getStringCellValue());
		}
		if (celda.getColumnIndex() == 1 && (celda.getCellTypeEnum() == CellType.STRING
				|| celda.getCellTypeEnum() == CellType.BLANK)) {
			datos.setNombreOrgano(celda.getStringCellValue());
		}
		if (celda.getColumnIndex() == 2 && (celda.getCellTypeEnum() == CellType.STRING
				|| celda.getCellTypeEnum() == CellType.BLANK)) {
			datos.setSiglas(celda.getStringCellValue());
		}
		if (celda.getColumnIndex() == 3 && (celda.getCellTypeEnum() == CellType.STRING
				|| celda.getCellTypeEnum() == CellType.BLANK)) {
			datos.setSiglasOrganoSuperior(celda.getStringCellValue());
		}
	}

	@Override
	public RespBase<Object> cargaMasivaOrganigrama(MyJsonWebToken token, List<OrganoExcelDTO> lista, Long entidadId) {// NOSONAR
		RespBase<RespOrganigrama> response = new RespBase<>();
		RespOrganigrama responsePayload = new RespOrganigrama();
		RespBase<Object> responseMasivo = new RespBase<>();
		ReqBase<ReqOrganigrama> organo = new ReqBase<>();
		Parametro limiteRegistro = generalRepository.buscarParametro(null, Constantes.LIMIT_MASIVO, null);
		lista.removeIf(x -> !StringUtils.isEmpty(x.getEliminarFila()) && x.isFlagRegistrar() == false);
		for (OrganoExcelDTO organigramaExcelDto : lista) {

			if (limiteRegistro.getCodigoNumero() >= lista.size()) {
				if (StringUtils.isEmpty(organigramaExcelDto.getObservacion())
						&& organigramaExcelDto.isFlagRegistrar()) {
					ReqOrganigrama organigrama = new ReqOrganigrama();
					organigrama.setEntidadId(entidadId);
					organigrama.setEstadoRegistro(organigramaExcelDto.getEstado().split("-")[0].trim());
					organigrama.setNivel(Long.parseLong(organigramaExcelDto.getNivel().split("-")[0].trim()));
					organigrama.setDescripcion(organigramaExcelDto.getNombreOrgano());
					organigrama.setSigla(organigramaExcelDto.getSigla());		
					organigrama.setNaturalezaOrgano(
							Long.parseLong(organigramaExcelDto.getNaturaleza().split("-")[0].trim()));
					if (organigramaExcelDto.getTipoOrgano().equalsIgnoreCase("Seleccionar")) {
						organigrama.setPadreOrganigramaId(null);
					} else {
						organigrama.setPadreOrganigramaId(
								Long.parseLong(organigramaExcelDto.getTipoOrgano().split("-")[0].trim()));
					}
					organigrama.setTipoOrganoUoId(Long.valueOf(variablesSistema.tipoOrgano));
					organigrama.setOrden(0);
					organigrama.setTipoDocumento(
							Integer.parseInt(organigramaExcelDto.getTipoDocumento().split("-")[0].trim()));
					organigrama.setNumeroDocumento(organigramaExcelDto.getNroDocumento());
					organigrama.setNombres(organigramaExcelDto.getNombres());
					organigrama.setApellidoPaterno(organigramaExcelDto.getApellidoPaterno());
					organigrama.setApellidoMaterno(organigramaExcelDto.getApellidoMaterno());
					organigrama.setPuesto(organigramaExcelDto.getPuesto());
					organigrama.setTelefono(organigramaExcelDto.getCelular());
					organigrama.setCorreo(organigramaExcelDto.getCorreoLaboral());
					organigrama.setPaisId(Long.parseLong(organigramaExcelDto.getPais().split("-")[0].trim()));
					organo.setPayload(organigrama);

					response = guardarOrganigrama(organo, token, null);// NOSONAR

					if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
						String mensajeResultado = "";
						for (String mensaje : response.getStatus().getError().getMessages()) {
							mensajeResultado = mensajeResultado + mensaje + ","; // NOSONAR
						}
						mensajeResultado = mensajeResultado.trim().substring(0, mensajeResultado.length() - 1);
						organigramaExcelDto.setObservacionResultado(mensajeResultado);
						responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
								organigramaExcelDto.getObservacionResultado());
						// return responseMasivo;

					} else {
						responsePayload.setOrganigrama(response.getPayload().getOrganigrama());
						responsePayload.setPersona(response.getPayload().getPersona());
						responseMasivo.setPayload(responsePayload);
					}

				} else {
					responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacion().isEmpty()));
				}
			} else {
				organigramaExcelDto
						.setObservacionResultado(Constantes.MENSAJE_LIMITE_MASIVO + limiteRegistro.getCodigoNumero());
				responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
						organigramaExcelDto.getObservacionResultado());
			}

		}

		responseMasivo.getStatus().setSuccess(Boolean.TRUE);

		return responseMasivo;
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> cargaMasivaOrganigramaSGM(MyJsonWebToken token, List<OrganigramaExcelDTO> lista,
			Long entidadId) {

		RespBase<RespOrganigrama> response = null;
		ReqBase<ReqOrganigrama> organo = new ReqBase<>();

		RespBase<Object> responseMasivo = new RespBase<>();

		List<Parametro> listTipoOrgano = generalRepository.buscarListaParametro(null, Constantes.PARAMETRO_TIPO_ORG,
				null);
		// lista.removeIf(x -> !StringUtils.isEmpty(x.getEliminarFila()) &&
		// x.isFlagRegistrar() == false);
		
		boolean validar = true;
		List<Organigrama> lstOrganigramaUpdate = new ArrayList<>();
		
		Organigrama findOrgano = new Organigrama();
		findOrgano.setEntidadId(entidadId);
		findOrgano.setEstadoRegistro(Constantes.ESTADO_ACTIVO);
		Example<Organigrama> example = Example.of(findOrgano);
		List<Organigrama> lstOrganigrama = organigramaRepository.findAll(example);
		
//			List<Organigrama> lstOrganigrama = organigramaRepository.findByEntidadId(entidadId);
			

			for (OrganigramaExcelDTO organigramaExcelDTO : lista) {

				if (!StringUtils.isEmpty(organigramaExcelDTO.getObservacion())
						&& !organigramaExcelDTO.isFlagRegistrar()) {
					responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacion().isEmpty()));
					validar = false;
				}

				String mensajeResultado = Constantes.VACIO;
				if (!StringUtils.trimToEmpty(organigramaExcelDTO.getSiglasOrganoSuperior()).isEmpty()
						&& !organigramaExcelDTO.getSiglasOrganoSuperior().equalsIgnoreCase(Constantes.NO_APLICA)) {
					if (!lista.stream().anyMatch(o -> o.getSiglas() != null
							&& o.getSiglas().contains(organigramaExcelDTO.getSiglasOrganoSuperior()))) {
						if (!lstOrganigrama.stream().anyMatch(o -> o.getSigla() != null
								&& o.getSigla().contains(organigramaExcelDTO.getSiglasOrganoSuperior()))) {
							mensajeResultado = mensajeResultado
									+ " La sigla de la UO padre no existe en la columna Siglas del Excel ni esta registrada previamente, ";
							validar = false;
						}
					}
				}

				if (StringUtils.trimToEmpty(organigramaExcelDTO.getSiglasOrganoSuperior()).isEmpty()) {
					mensajeResultado = mensajeResultado + "En caso no tuviese UO padre debe registrar NO APLICA, ";
					validar = false;
				}

				if (Constantes.SELECCIONAR
						.equalsIgnoreCase(StringUtils.trimToEmpty(organigramaExcelDTO.getTipoOrgano()))) {
					mensajeResultado = mensajeResultado + "Seleccione un tipo de organo, ";
					validar = false;
				}

				if (!StringUtils.trimToEmpty(organigramaExcelDTO.getTipoOrgano()).isEmpty() && StringUtils.trimToEmpty(organigramaExcelDTO.getTipoOrgano()).substring(0, 1).equals("0")) {
					mensajeResultado = mensajeResultado + "Seleccione un tipo de organo, ";
					validar = false;
				}
				
				if (StringUtils.trimToEmpty(organigramaExcelDTO.getNombreOrgano()).isEmpty()) {
					mensajeResultado = mensajeResultado + "Ingrese un organo/unidad organica /sub unidad organica, ";
					validar = false;
				} else {
					for (Organigrama organigrama : lstOrganigrama) {
						if (StringUtils.trimToEmpty(organigrama.getDescripcion())
								.equalsIgnoreCase(StringUtils.trimToEmpty(organigramaExcelDTO.getNombreOrgano()))) {
							mensajeResultado = mensajeResultado + " Ya existe una UO con el nombre "
									+ organigramaExcelDTO.getNombreOrgano() + ",";
							validar = false;
						}

					}

				}

				if (StringUtils.trimToEmpty(organigramaExcelDTO.getSiglas()).isEmpty()) {
					mensajeResultado = mensajeResultado + "Ingrese una sigla para la UO.";
					validar = false;
				} else {
					for (Organigrama organigrama : lstOrganigrama) {
						if (StringUtils.trimToEmpty(organigrama.getSigla())
								.equalsIgnoreCase(StringUtils.trimToEmpty(organigramaExcelDTO.getSiglas()))) {
							mensajeResultado = mensajeResultado + " Ya existe una UO con la sigla "
									+ organigramaExcelDTO.getSiglas() + ",";
							validar = false;
						}
					}

				}

				if (!StringUtils.trimToEmpty(organigramaExcelDTO.getNombreOrgano()).isEmpty()) {

					long repetidosUo = lista.stream()
							.filter(o -> o.getNombreOrgano().equalsIgnoreCase(organigramaExcelDTO.getNombreOrgano()))
							.count();
					if (repetidosUo > 1) {
						mensajeResultado = mensajeResultado + " El nombre de la UO "
								+ organigramaExcelDTO.getNombreOrgano() + " se repite " + repetidosUo + " veces,";
						validar = false;
					}

				}
				if (!StringUtils.trimToEmpty(organigramaExcelDTO.getSiglas()).isEmpty()) {

					long repetidosSigla = lista.stream()
							.filter(o -> o.getSiglas().equalsIgnoreCase(organigramaExcelDTO.getSiglas())).count();
					if (repetidosSigla > 1) {
						mensajeResultado = mensajeResultado + " La Sigla " + organigramaExcelDTO.getSiglas()
								+ " se repite " + repetidosSigla + " veces,";
						validar = false;

					}

				}

				if (!mensajeResultado.isEmpty()) {
					organigramaExcelDTO.setObservacionResultado(mensajeResultado);
				}
			}

			if (validar) {
				for (OrganigramaExcelDTO o : lista) {
					if (0 != Long.parseLong(o.getTipoOrgano().split("-")[0].trim())) {
						ReqOrganigrama organigrama = new ReqOrganigrama();
						organigrama.setEntidadId(entidadId);
						organigrama.setNaturalezaOrgano(Long.parseLong(o.getTipoOrgano().split("-")[0].trim()));
						organigrama.setDescripcion(o.getNombreOrgano());
						organigrama.setSigla(o.getSiglas());			
						organigrama.setTipoOrganoUoId(obtenerTipoOrgano(listTipoOrgano, o.getSiglasOrganoSuperior()));

						organo.setPayload(organigrama);
						response = guardarOrganigramaSGM(organo, token);
						lstOrganigramaUpdate.add(response.getPayload().getOrganigrama());
					}

				}

				for (OrganigramaExcelDTO o1 : lista) { // Actualizar organigrama superior
					if (o1.getSiglasOrganoSuperior() != null && !o1.getSiglasOrganoSuperior().isEmpty()
							&& !o1.getSiglasOrganoSuperior().equalsIgnoreCase(Constantes.NO_APLICA)) {

						Optional<Long> organigramaIdPadre = lstOrganigramaUpdate.stream()
								.filter(o -> o.getSigla().equalsIgnoreCase(o1.getSiglasOrganoSuperior()))
								.map(o -> o.getOrganigramaId()).findFirst();

						if (!organigramaIdPadre.isPresent()) {
							organigramaIdPadre = lstOrganigrama.stream()
									.filter(o -> o.getSigla().equals(o1.getSiglasOrganoSuperior()))
									.map(o -> o.getOrganigramaId()).findFirst();
						}

						Organigrama organigramaUpdate = lstOrganigramaUpdate.stream()
								.filter(o -> o.getSigla().equalsIgnoreCase(o1.getSiglas())).findFirst().get();

						organigramaUpdate.setPadreOrganigramaId(organigramaIdPadre.get());

						organigramaRepository.save(organigramaUpdate);
					}
				}
			
				responseMasivo.getStatus().setSuccess(Boolean.TRUE);
			}else {
				responseMasivo.getStatus().setSuccess(Boolean.FALSE);
			}
		return responseMasivo;
	}

	private Long obtenerTipoOrgano(List<Parametro> listTipoOrgano, String siglasOrganoSup) {

		Long tipoOrganoUO_id = 0L;

		if (siglasOrganoSup.equals(Constantes.NO_APLICA)) {
			Optional<Integer> tip = listTipoOrgano.stream()
					.filter(p -> p.getCodigoTexto().equalsIgnoreCase(Constantes.TIPO_ORGANO))
					.map(p -> p.getParametroId()).findFirst();
			if (tip.isPresent()) {
				tipoOrganoUO_id = (long) tip.get();
			}

		} else {
			Optional<Integer> tip2 = listTipoOrgano.stream()
					.filter(p -> p.getCodigoTexto().equalsIgnoreCase(Constantes.TIPO_UNIDAD_ORGANICA))
					.map(p -> p.getParametroId()).findFirst();

			if (tip2.isPresent()) {
				tipoOrganoUO_id = (long) tip2.get();
			}
		}

		return tipoOrganoUO_id;
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespOrganigrama> guardarOrganigramaSGM(ReqBase<ReqOrganigrama> request, MyJsonWebToken token) {
		Organigrama organigrama = new Organigrama();

		organigrama.setEntidadId(request.getPayload().getEntidadId());
		organigrama.setNaturalezaOrgano(request.getPayload().getNaturalezaOrgano());
		organigrama.setDescripcion(request.getPayload().getDescripcion());
		organigrama.setSigla(request.getPayload().getSigla());			
		organigrama.setTipoOrganoUoId(request.getPayload().getTipoOrganoUoId());

		organigrama.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());

		organigrama = organigramaRepository.save(organigrama);

		RespOrganigrama payload = new RespOrganigrama();
		payload.setOrganigrama(organigrama);

		return new RespBase<RespOrganigrama>().ok(payload);
	}

	@Override
	public RespBase<RespObtenerServidorCivil> selectServidoresCiviles(Map<String, Object> parametroMap) {
		List<ServidorCivilDTO> ltaServidorCivilFilter = gestionRepository.selectServidoresCiviles(parametroMap);
		RespObtenerServidorCivil respPayload = new RespObtenerServidorCivil();
		respPayload.setListaServidorCivil(ltaServidorCivilFilter);
		return new RespBase<RespObtenerServidorCivil>().ok(respPayload);
	}

	@Override
	public RespBase<RespObtenerGestionOrganigrama> selectGestionOrganigrama(Map<String, Object> parametroMap) {
		List<GestionOrganigramaDTO> ltaGestionOrganigramaFilter = gestionRepository
				.selectGestionOrganigrama(parametroMap);
		RespObtenerGestionOrganigrama respPayload = new RespObtenerGestionOrganigrama();
		respPayload.setListaGestionOrganigrama(ltaGestionOrganigramaFilter);
		return new RespBase<RespObtenerGestionOrganigrama>().ok(respPayload);
	}

	@Override
	public List<OrganigramaExcelDTO> obetenerCodigoCombo(byte[] combos, List<OrganigramaExcelDTO> lista) {
		try {
			InputStream input = new ByteArrayInputStream(combos);
			XSSFWorkbook workbook = new XSSFWorkbook(input);
			XSSFSheet hojaCodigoCombos = workbook.getSheetAt(2);
			int columnas = hojaCodigoCombos.getRow(0).getLastCellNum();
			int indiceLista = 0;
			for (int i = 1; i <= lista.size(); i++) {
				XSSFRow filaInicial = hojaCodigoCombos.getRow(i);
				for (int j = 0; j < columnas; j++) {
					XSSFCell celda = filaInicial.getCell(j);
					validarCombosExcel(celda, lista, indiceLista);
				}
				indiceLista++;
			}
			workbook.close();
		} catch (Exception e) {
			LOGGER.info("ERROR AL OBTENER EL CODIGO" + e.getMessage());
			e.getMessage();
		}

		return lista;
	}

	private void validarCombosExcel(XSSFCell celda, List<OrganigramaExcelDTO> lista, int indiceLista) {
		if (celda.getColumnIndex() == 0 && celda.getCellTypeEnum() == CellType.FORMULA) {
			Long id = 0L;
			String tipoOrgano = "";
			id = (long) celda.getNumericCellValue();
			tipoOrgano = id.toString() + " - " + StringUtils.trimToEmpty(lista.get(indiceLista).getTipoOrgano());
			lista.get(indiceLista).setTipoOrgano(tipoOrgano);
		}

	}

}