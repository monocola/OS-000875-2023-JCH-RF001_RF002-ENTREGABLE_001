package pe.gob.servir.entidad.service.impl;

import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
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
import pe.gob.servir.entidad.model.ComboUnidadOrganica;
import pe.gob.servir.entidad.model.Organigrama;
import pe.gob.servir.entidad.model.OrganigramaDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.UnidadOrganicaDTO;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.OrganigramaRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.UnidadOrganicaExcelDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerUnidadOrganica;
import pe.gob.servir.entidad.response.RespOrganigrama;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.OrganigramaService;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.service.UnidadOrganicaService;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class UnidadOrganicaServiceImpl implements UnidadOrganicaService{
	
	private static final Logger LOGGER = Logger.getLogger(UnidadOrganicaServiceImpl.class);
	
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
	private BeanAdapterOrganigrama beanAdapterOrganigrama;
	
	@Autowired
	private OrganigramaService organigramaService;
	
	@Transactional(transactionManager = "entidadTransactionManager")
	@SuppressWarnings("rawtypes")
	@Override
	public RespBase<RespOrganigrama> guardarUnidadOrganica(ReqBase<ReqOrganigrama> request, MyJsonWebToken token,Long organigramaId) {//NOSONAR
		RespBase<RespOrganigrama> response = new RespBase<>();
		Organigrama organigrama = null;
		List<OrganigramaDTO> ltaValid = gestionRepository.buscarOrganigramaByPersona(request.getPayload().getTipoDocumento(), request.getPayload().getNumeroDocumento());
		if (organigramaId != null) {
			Optional<Organigrama> organimgramaFind = organigramaRepository.findById(organigramaId);
			if (organimgramaFind.isPresent()) {
				if(ltaValid !=  null && !ltaValid.isEmpty()) {
					if(ltaValid.get(0).getEntidadId().longValue() != organimgramaFind.get().getEntidadId().longValue()) {//NOSONAR
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
								"Ya existe un Representante con el Nro Documento Ingresado.!");
						return response;
					}
				}				
				organigrama = organimgramaFind.get();
				organigrama.setCampoSegUpd(request.getPayload().getEstadoRegistro(), token.getUsuario().getUsuario(),
						Instant.now());
			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						"No Existe la unidadOrganicaId Ingresado");
				return response;
			}
		} else {
			if(ltaValid !=  null && !ltaValid.isEmpty()) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						"Ya existe un Representante con el Nro Documento Ingresado.!");
				return response;
			}
			organigrama = new Organigrama();
			organigrama.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
		}		
		PersonaDTO personaResponsable = beanAdapterOrganigrama.adapToPersonaDTO(request);
		
		if(request.getPayload().getTipoDocumento().intValue() == variablesSistema.tipoDocumentoDni){
			personaResponsable.setPaisId(variablesSistema.idPaisPeru);
		}else{
			personaResponsable.setPaisId(request.getPayload().getPaisId());			
		}		
		RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
		ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();
		ParametrosUtil.setearPersonaNatural(apiPersonaNatural, personaResponsable);
		personaNaturalResquest.setPayload(apiPersonaNatural);
		RespBase<Object> responseWS = personaService.obtenerInsertarPersonaNuevaVersion(variablesSistema.tipoPersonaNatural,
				personaResponsable.getTipoDocumento(), personaResponsable.getNumeroDocumento(), personaNaturalResquest);
		if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					"El servicio de la API Persona no responde, por favor verifique que el servicio este funcionando correctamente o que los Nombres y Apellidos esten en mayusculas y no tengan numeros");
			return response;
		}
		RespApiPersona personaResponse = (RespApiPersona) responseWS.getPayload();
		
		organigrama.setPersonaResponsableId(personaResponse.getPersona().getPersonaId());
		organigrama.setTelefonoId((personaResponse.getTelefonos() != null && !personaResponse.getTelefonos().isEmpty())? personaResponse.getTelefonos().get(0).getTelefonoId() : null);
		organigrama.setCorreoId((personaResponse.getCorreos() != null && !personaResponse.getCorreos().isEmpty())? personaResponse.getCorreos().get(0).getCorreoId() : null);
		organigrama.setAreaId(request.getPayload().getAreaId());
		organigrama.setDescripcion(request.getPayload().getDescripcion());
		organigrama.setDescripcionCorta(request.getPayload().getDescripcionCorta());
		organigrama.setEntidadId(request.getPayload().getEntidadId());
		organigrama.setNaturalezaOrgano(request.getPayload().getNaturalezaOrgano());
		organigrama.setNivel(request.getPayload().getNivel());
		organigrama.setNivelGobiernoId(request.getPayload().getNivelGobiernoId());
		organigrama.setOrden(request.getPayload().getOrden());
		organigrama.setPadreOrganigramaId(request.getPayload().getPadreOrganigramaId());
		organigrama.setPuesto(request.getPayload().getPuesto());
		organigrama.setSedeId(request.getPayload().getSedeId());
		organigrama.setSigla(request.getPayload().getSigla());
		organigrama.setTipoOrganoUoId(request.getPayload().getTipoOrganoUoId());
		organigrama.setEstadoRegistro(request.getPayload().getEstadoRegistro());
		
		organigramaRepository.save(organigrama);
		RespOrganigrama payload = new RespOrganigrama();
		payload.setOrganigrama(organigrama);
		payload.setPersona(personaResponse);
		return new RespBase<RespOrganigrama>().ok(payload);
	}

	@Override
	public RespBase<RespObtenerUnidadOrganica> buscarUnidadOrganica(Long entidadId) {
		Map<String, Object> parametroMap = new HashMap<>();		
		parametroMap.put(Constantes.ENTIDADID, entidadId);
		List<ComboUnidadOrganica> lista= organigramaService.buscarUnidadesOrganicasPorEntidad(parametroMap).getPayload().getListaComboUnidadOrganica();
		List<UnidadOrganicaDTO> listaUnidadOrganica = new ArrayList<UnidadOrganicaDTO>();
		for (int i = 0; i < lista.size(); i++) {
			UnidadOrganicaDTO uo = new UnidadOrganicaDTO();
			uo.setOrganigramaId(lista.get(i).getId());
			uo.setUnidadOrganica(lista.get(i).getDescripcion());
			uo.setSigla(lista.get(i).getSigla());				
			uo.setTipoOrganoUoId(lista.get(i).getTipoOrganoId()); 
		    uo.setPadreOrganigramaId(lista.get(i).getUoSupId());	
		    uo.setEstadoRegistro(lista.get(i).getEstadoRegistro());
			listaUnidadOrganica.add(uo);
		}
		RespObtenerUnidadOrganica respPayload = new RespObtenerUnidadOrganica();
		respPayload.setListaUnidadOrganica(listaUnidadOrganica);
		return new RespBase<RespObtenerUnidadOrganica>().ok(respPayload);
	}

	@Override
	public RespBase<Object> eliminarUnidadOrganica(MyJsonWebToken token, Long unidadOrganicaId, String estado) {
		RespBase<Object> response = new RespBase<>();
		Optional<Organigrama> organimgramaFind = organigramaRepository.findById(unidadOrganicaId);
		if (organimgramaFind.isPresent()) {
			Organigrama organigrama = organimgramaFind.get();
			if(estado.equals(EstadoRegistro.INACTIVO.getCodigo())) {
				Organigrama organigramaFilter = new Organigrama();
				organigramaFilter.setPadreOrganigramaId(organigrama.getOrganigramaId());
				organigramaFilter.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
				Example<Organigrama> example = Example.of(organigramaFilter);
				List<Organigrama> ltaUnidadFilter = organigramaRepository.findAll(example);
				if(!ltaUnidadFilter.isEmpty()) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "No se puede eliminar, aún tiene órganos dependientes");
					return response; 
				}
			}
			organigrama.setEstadoRegistro(estado);
			organigrama.setCampoSegUpd(EstadoRegistro.INACTIVO.getCodigo(), token.getUsuario().getUsuario(),
					Instant.now());
			organigramaRepository.save(organigrama);	
			return new RespBase<Object>().ok(organigrama);
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "No Existe el unidadOrganicaId Ingresado");
		}
		return response;
	}

	@Override
	public List<UnidadOrganicaExcelDTO> validarCargaMasivaUnidadOrganica(InputStream uploadedInputStream) {
		try {
			ExcelUtil<UnidadOrganicaExcelDTO> file = new ExcelUtil<>(UnidadOrganicaExcelDTO::new);
			List<UnidadOrganicaExcelDTO> listaUnidadOrganica = file.utilExcelToPojo(uploadedInputStream, Constantes.HOJA_EXCEL_CERO);//NOSONAR
			return listaUnidadOrganica;

		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			
		}
		return null;
	}

	@Override
	public RespBase<Object> cargaMasivaUnidadOrganica(MyJsonWebToken token, List<UnidadOrganicaExcelDTO> lista,Long entidadId) {
		RespBase<RespOrganigrama> response;
		RespOrganigrama responsePayload =  new RespOrganigrama();
		RespBase<Object> responseMasivo = new RespBase<>();
		ReqBase<ReqOrganigrama> unidadOrganicaPayload =  null;
		Parametro limiteRegistro=generalRepository.buscarParametro(null, Constantes.LIMIT_MASIVO, null);
		lista.removeIf(x -> !StringUtils.isEmpty(x.getEliminarFila()) && x.isFlagRegistrar() == false); 
		for (UnidadOrganicaExcelDTO unidadOrganicaExcelDto : lista) {
			if (limiteRegistro.getCodigoNumero()>=lista.size()) {
				if (StringUtils.isEmpty(unidadOrganicaExcelDto.getObservacion())&& unidadOrganicaExcelDto.isFlagRegistrar()) {
					unidadOrganicaPayload = beanAdapterOrganigrama.adapTobeanUnidadOrganicaPayload(entidadId, unidadOrganicaExcelDto);
					response = guardarUnidadOrganica(unidadOrganicaPayload, token, null);// NOSONAR
					if (Boolean.FALSE.equals(response.getStatus().getSuccess())) {
						String mensajeResultado = "";
						for (String mensaje : response.getStatus().getError().getMessages()) {
							mensajeResultado = mensajeResultado + mensaje + ",";// NOSONAR
						}
						mensajeResultado = mensajeResultado.trim().substring(0, mensajeResultado.length() - 1);
						unidadOrganicaExcelDto.setObservacionResultado(mensajeResultado);
						responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
								unidadOrganicaExcelDto.getObservacionResultado());
					} else {
						responsePayload.setOrganigrama(response.getPayload().getOrganigrama());
						responsePayload.setPersona(response.getPayload().getPersona());
						responseMasivo.setPayload(responsePayload);
					}

				} else {responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacion().isEmpty()));}
			} else {
				unidadOrganicaExcelDto.setObservacionResultado(Constantes.MENSAJE_LIMITE_MASIVO+limiteRegistro.getCodigoNumero());
				responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
						unidadOrganicaExcelDto.getObservacionResultado());				
			}
		}		
		responseMasivo.getStatus().setSuccess(Boolean.TRUE);
			
		return responseMasivo;		
	}

}
