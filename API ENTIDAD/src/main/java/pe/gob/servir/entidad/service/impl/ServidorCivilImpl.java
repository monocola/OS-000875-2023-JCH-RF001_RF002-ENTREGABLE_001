package pe.gob.servir.entidad.service.impl;

import java.io.InputStream;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;

import javax.validation.Valid;

import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.lang3.StringUtils;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import feign.FeignException;
import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiFileServerDTO;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.model.Correo;
import pe.gob.servir.entidad.model.CorreoPersonaDTO;
import pe.gob.servir.entidad.model.DatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.model.DetUnidadOrganica;
import pe.gob.servir.entidad.model.Empleado;
import pe.gob.servir.entidad.model.EvaluadorEvaluadoDTO;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.NuevoEvaluadorDTO;
import pe.gob.servir.entidad.model.Organigrama;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.ParticipanteDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadoresServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadosServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteServidorCivilDTO;
import pe.gob.servir.entidad.model.Persona;
import pe.gob.servir.entidad.model.PersonasPuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.Puesto;
import pe.gob.servir.entidad.model.PuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO;
import pe.gob.servir.entidad.model.RespBuscarParticipantesEntidadServidorCivilDTO;
import pe.gob.servir.entidad.model.RespParticipanteDTO;
import pe.gob.servir.entidad.model.UnidadOrganica;
import pe.gob.servir.entidad.repository.CorreoRepository;
import pe.gob.servir.entidad.repository.DetEncargaturaRepository;
import pe.gob.servir.entidad.repository.DetUnidadOrganicaRepository;
import pe.gob.servir.entidad.repository.EditarParticipanteRepository;
import pe.gob.servir.entidad.repository.EmpleadoRepository;
import pe.gob.servir.entidad.repository.EntidadRepository;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.OrganigramaRepository;
import pe.gob.servir.entidad.repository.PersonaRepository;
import pe.gob.servir.entidad.repository.PuestoRepository;
import pe.gob.servir.entidad.repository.ServidoresCivilRepository;
import pe.gob.servir.entidad.repository.UnidadOrganicaRepository;
import pe.gob.servir.entidad.request.ReqActualizaNuevoEvaluadorServidorCivil;
import pe.gob.servir.entidad.request.ReqActualizarPuesto;
import pe.gob.servir.entidad.request.ReqAgregarPuesto;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEditaServidorCivil;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.EditParticipanteGDRDTO;
import pe.gob.servir.entidad.request.dto.LogoDTO;
import pe.gob.servir.entidad.request.dto.NuevoEvaluadorServidorCivilEdicionDTO;
import pe.gob.servir.entidad.request.dto.PuestoActualizarDTO;
import pe.gob.servir.entidad.request.dto.PuestoAgregarDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilEdicionDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilExcelDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilGDRDTO;
import pe.gob.servir.entidad.response.RespApiCorreoPersona;
import pe.gob.servir.entidad.response.RespApiFile;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespBuscarParticipante;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadoEvaluador;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadoresServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadosServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarParticipanteServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarPersonasPuestoUoServidorCivil;
import pe.gob.servir.entidad.response.RespObtenePuestoUoServidorCivil;
import pe.gob.servir.entidad.response.RespObtenerCorreo;
import pe.gob.servir.entidad.response.RespObtenerDatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.response.RespObtenerTelefono;
import pe.gob.servir.entidad.response.RespPersonaServidorCivil;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.ServidorCivilService;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class ServidorCivilImpl implements ServidorCivilService {

	private static final Logger LOGGER = Logger.getLogger(ServidorCivilImpl.class);

	@Autowired
	private BeanAdapterServidorCivil adapterServidorCivil;

	@Autowired
	ServidoresCivilRepository servidorCivilRepository;

	@Autowired
	DetUnidadOrganicaRepository detUnidadOrganicaRepository;

	@Autowired
	DetEncargaturaRepository detEncargaturaRepository;

	@Autowired
	private MaestraApiClient maestraApiClient;

	@Autowired
	private GeneralRepository generalRepositorySP;

	@Autowired
	GeneralRepository generalRepository;

	@Autowired
	BeanAdapterServidorCivil beanAdapter;

	@Autowired
	PersonaApiClient personaApiClient;

	@Autowired
	EmpleadoRepository empleadoRepository;

	@Autowired
	PersonaRepository personaRepository;

	@Autowired
	CorreoRepository correoRepository;

	@Autowired
	OrganigramaRepository organigramaRepository;

	@Autowired
	UnidadOrganicaRepository unidadOrganicaRepository;

	@Autowired
	PuestoRepository puestoRepository;

	@Autowired
	EditarParticipanteRepository editarParticipanteRepository;

	@Autowired
	EntidadRepository entidadRepository;

	@Override
	public RespBase<GenericResponseMessage> crearServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request,
			MyJsonWebToken jwt) throws Exception {
		return crearServidorCivil(request, jwt, false);
	}

		@Override
	public RespBase<GenericResponseMessage> crearServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request,
			MyJsonWebToken jwt, boolean esParaMasivo) throws Exception {
		boolean correo = ParametrosUtil
				.validarCorreo(StringUtils.trimToEmpty(request.getPayload().getServidorCivil().getCorreoElectronico()));

		if (correo) {
			
			Long puestoId = request.getPayload().getServidorCivil().getPuestoId();
			Long organigramaId = request.getPayload().getServidorCivil().getOrganoId();
			
			LOGGER.info("[RESPONSE PUESTO ID: ] " + puestoId);
			LOGGER.info("[RESPONSE ORGANIGRAMA ID: ] " + organigramaId);
			Optional<Puesto> oPuesto = puestoRepository.findById(puestoId);
			///System.out.println("REQUEEEEEEEEEEST1111");
			///System.out.println("REQUEEEEEEEEEEST1111||"+ oPuesto.toString());
			
			if (oPuesto.isPresent()) {
				///System.out.println("REQUEEEEEEEEEEST1111" + oPuesto.isPresent());
				String esJefe = oPuesto.get().getEsJefe();
				if(esJefe.equals("S")) {
					Optional<Organigrama> oOrganigrama = organigramaRepository.findById(organigramaId);
					if (oOrganigrama.isPresent()) {
						
						if(oOrganigrama.get().getPersonaResponsableId() != null) {
							RespBase<GenericResponseMessage> respuesta = new RespBase<>();
							return ParametrosUtil.setearResponse(respuesta, Boolean.FALSE, "Ya existe un servidor civil asignado para el puesto seleccionado");								
						}					
					}					
					request.getPayload().getServidorCivil().setResponsable("S");
				}
			 }
			///System.out.println("REQUEEEEEEEEEEST22222");
			///System.out.println("REQUEEEEEEEEEEST22222"+ request.getPayload().getServidorCivil());
			///System.out.println("REQUEEEEEEEEEEST22222" + request.getPayload().getServidorCivil().getNumeroDocumento());
			RespBase<GenericResponseMessage> responseServidor = servidorCivilRepository.insertarServidorCivil(request,
					jwt, esParaMasivo);
			
			Long idDetalleUOPersonaAsiganda = request.getPayload().getServidorCivil().getIdDetUOPersonaAsingada();

			if (Boolean.TRUE.equals(responseServidor.getStatus().getSuccess())
					&& responseServidor.getPayload().getCodigo().equals(Constantes.VALIDACION_CESE_SERVIDOR)
					&& idDetalleUOPersonaAsiganda != null) {
				
				ReqBase<DetUnidadOrganica> requestDetUnOrga = beanAdapter.adapToRequestDetUnidadOrganica(
						idDetalleUOPersonaAsiganda, request.getPayload().getServidorCivil().getFechaInicio());
				cesarServidorCivil(requestDetUnOrga, jwt);
			}
			return responseServidor; 

		} else {
			RespBase<GenericResponseMessage> respuesta = new RespBase<>();
			return ParametrosUtil.setearResponse(respuesta, Boolean.FALSE, "Estructura de correo no valido");
		}

	}

	@Override
	public RespBase<Object> cesarServidorCivil(@Valid ReqBase<DetUnidadOrganica> request, MyJsonWebToken token) {

		RespBase<Object> response = new RespBase<>();

		Optional<DetUnidadOrganica> detUO = detUnidadOrganicaRepository
				.findById(request.getPayload().getDetUnidadOrganicaId());

		if (detUO.isPresent()) {
			DetUnidadOrganica servCivil = null;
			Parametro estadoServCiv = generalRepository.buscarParametro(null, Constantes.ESTADO_SERVIDOR_CIVIL,
					Constantes.CESADO);

			if (detUO.get().getFechaCesePuesto() == null) {
				detUO.get().setFechaCesePuesto(request.getPayload().getFechaCesePuesto());
				detUO.get().setCampoSegUpd(String.valueOf(estadoServCiv.getCodigoNumero()),
						token.getUsuario().getUsuario(), Instant.now());

				servCivil = detUnidadOrganicaRepository.save(detUO.get());
				response.getStatus().setSuccess(Boolean.TRUE);
				response.setPayload(servCivil);
			} else {
				return ParametrosUtil.setearResponse(response, Boolean.FALSE,
						"El servidor civil ya se encuentra cesado");
			}

		} else {
			return ParametrosUtil.setearResponse(response, Boolean.FALSE, " NO SE ENCONTRO EL SERVIDOR CIVIL");
		}

		return response;
	}

	@Override
	public RespBase<RespObtenerDatosPersonalesServidorCivilDTO> obtenerDatosPersonalesServidorCivil(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<DatosPersonalesServidorCivilDTO> lista = servidorCivilRepository
					.obtenerDatosPersonalesServidorCivil(parametroMap);

			RespPersonaServidorCivil out = new RespPersonaServidorCivil();
			if (!CollectionUtils.isEmpty(lista)) {
				for (DatosPersonalesServidorCivilDTO inServidor : lista) {
					adapterServidorCivil.adapToBeanResponseServidorCivil(out, inServidor);
				}
			}

			RespObtenerDatosPersonalesServidorCivilDTO respPayload = new RespObtenerDatosPersonalesServidorCivilDTO();
			respPayload.setDatosPersonalesServidorCivil(out);
			return new RespBase<RespObtenerDatosPersonalesServidorCivilDTO>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespObtenePuestoUoServidorCivil> obtenerPuestoUoServidorCivil(Map<String, Object> parametroMap)
			throws ValidationException {
		try {
			List<PuestoUoServidorCivilDTO> lista = servidorCivilRepository.obtenerPuestoUoServidorCivil(parametroMap);
			RespObtenePuestoUoServidorCivil respPayload = new RespObtenePuestoUoServidorCivil();
			respPayload.setListaPuestoUoServidorCivil(lista);
			return new RespBase<RespObtenePuestoUoServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> eliminarServidorCivil(MyJsonWebToken jwt, Long detUnidadOrganicaId, String estado) {

		RespBase<Object> response = new RespBase<>();

		try {
			Optional<DetUnidadOrganica> detUO = detUnidadOrganicaRepository.findById(detUnidadOrganicaId);
			if (detUO.isPresent()) {
				DetUnidadOrganica servCivil = new DetUnidadOrganica();

				detUO.get().setCampoSegUpd(estado, jwt.getUsuario().getUsuario(), Instant.now());
				servCivil = detUnidadOrganicaRepository.save(detUO.get());
				
				Long organigramaId = detUO.get().getOrganigramaId();
				String esJefe = detUO.get().getResponsable();
				if(esJefe.equals("S")) {
					Optional<Organigrama> oOrganigrama = organigramaRepository.findById(organigramaId);
					if (oOrganigrama.isPresent()) {
						oOrganigrama.get().setPersonaResponsableId(null);
						organigramaRepository.save(oOrganigrama.get());	
					}
				}
 
				List<Empleado> lista = new ArrayList<Empleado>();
				lista = empleadoRepository.findAllByEntidadIdAndPersonaId(
							detUO.get().getEntidadId(),
							detUO.get().getPersonaId());
				if (lista != null && !lista.isEmpty()) {
					for (Empleado empleado : lista) {
						Empleado empleadoUpdate = new Empleado();
						empleado.setCampoSegUpd(estado, jwt.getUsuario().getUsuario(), Instant.now());
						empleadoUpdate = empleadoRepository.save(empleado);
						LOGGER.info("[empleadoUpdate: ] " + JsonUtil.convertirObjetoACadenaJson(empleadoUpdate));
					}
				}

				response.getStatus().setSuccess(Boolean.TRUE);
				response.setPayload(servCivil);

			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, " NO SE ENCONTRO EL SERVIDOR CIVIL");
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}

		return response;
	}

	@Override
	public RespBase<RespBuscarPersonasPuestoUoServidorCivil> buscarPersonasPuestoUoServidorCivil(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<PersonasPuestoUoServidorCivilDTO> lista = servidorCivilRepository
					.buscarPersonasPuestoUoServidorCivil(parametroMap);
			RespBuscarPersonasPuestoUoServidorCivil respPayload = new RespBuscarPersonasPuestoUoServidorCivil();
			respPayload.setListaPersonasPuestoUoServidorCivil(lista);
			return new RespBase<RespBuscarPersonasPuestoUoServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteServidorCivil> buscarParticipanteServidorCivil(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<ParticipanteServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipanteServidorCivil(parametroMap);
			RespBuscarParticipanteServidorCivil respPayload = new RespBuscarParticipanteServidorCivil();
			respPayload.setListaParticipanteServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}
	
	@Override
	public RespBase<RespBuscarParticipanteServidorCivil> buscarParticipanteServidorCivilNoActivos(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<ParticipanteServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipanteServidorCivilNoActivos(parametroMap);
			RespBuscarParticipanteServidorCivil respPayload = new RespBuscarParticipanteServidorCivil();
			respPayload.setListaParticipanteServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadoresServidorCivil> buscarParticipantesEvaluadoresServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException {
		try {
			List<ParticipanteEvaluadoresServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadoresServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadoresServidorCivil respPayload = new RespBuscarParticipanteEvaluadoresServidorCivil();
			respPayload.setListaParticipanteEvaluadoresServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteEvaluadoresServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadosServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadosServidorCivil respPayload = new RespBuscarParticipanteEvaluadosServidorCivil();
			respPayload.setListaParticipanteEvaluadosServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosNoMandoMedioServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadosNoMandoMedioServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadosServidorCivil respPayload = new RespBuscarParticipanteEvaluadosServidorCivil();
			respPayload.setListaParticipanteEvaluadosServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesMandoMedioServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesMandoMedioServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadosServidorCivil respPayload = new RespBuscarParticipanteEvaluadosServidorCivil();
			respPayload.setListaParticipanteEvaluadosServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil> buscarEvaluadosSinEvaluadoresServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarEvaluadosSinEvaluadoresServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil respPayload = new RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil();
			respPayload.setCantidadSinEvaluar(lista != null ? lista.size() : 0);
			respPayload.setListaParticipanteEvaluadosSinEvaluador(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadosServidorCivil respPayload = new RespBuscarParticipanteEvaluadosServidorCivil();
			respPayload.setListaParticipanteEvaluadosServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> actualizarParticiapantesNuevoEvaluadorServidorCivil(@Valid ReqBase<ReqActualizaNuevoEvaluadorServidorCivil> request, MyJsonWebToken jwt)
			throws ValidationException {
		try {
			NuevoEvaluadorServidorCivilEdicionDTO bean = new NuevoEvaluadorServidorCivilEdicionDTO();
			RespBase<Object> response = new RespBase<>();

			bean.setEvaluadorMMdetUoId(request.getPayload().getEvaluadorMMdetUoId());
			bean.setEntidadId(request.getPayload().getEntidadId());
			bean.setUoId(request.getPayload().getUoId());
			bean.setUsuario(jwt.getUsuario().getUsuario());
			bean.setEvaluados(request.getPayload().getEvaluados());
			LOGGER.info(Constantes.LOG_BEAN + JsonUtil.convertirObjetoACadenaJson(bean));

			Boolean flagSave = actualizarParticipantes(bean);

			response = (flagSave.equals(Boolean.TRUE)
					? ParametrosUtil.setearResponse(response, Boolean.TRUE, " Se realizó la actualización exitosamente",
							null)
					: ParametrosUtil.setearResponse(response, Boolean.FALSE,
							" No se actualizó unos de los participantes evaluados"));
			LOGGER.info(Constantes.LOG_RESPONSE + JsonUtil.convertirObjetoACadenaJson(response));

			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	
	@Override
	public RespBase<Object> actualizarParticiapantesEvaluadorServidorCivil(@Valid ReqBase<ReqActualizaNuevoEvaluadorServidorCivil> request, MyJsonWebToken jwt)
			throws ValidationException {
		try {
			NuevoEvaluadorServidorCivilEdicionDTO bean = new NuevoEvaluadorServidorCivilEdicionDTO();
			RespBase<Object> response = new RespBase<>();

			bean.setEvaluadorMMdetUoId(request.getPayload().getEvaluadorMMdetUoId());
			bean.setEntidadId(request.getPayload().getEntidadId());
			bean.setUoId(request.getPayload().getUoId());
			bean.setUsuario(jwt.getUsuario().getUsuario());
			bean.setEvaluados(request.getPayload().getEvaluados());
			LOGGER.info(Constantes.LOG_BEAN + JsonUtil.convertirObjetoACadenaJson(bean));

			Boolean flagSave = actualizarParticipantesEvaluado(bean);

			response = (flagSave.equals(Boolean.TRUE)
					? ParametrosUtil.setearResponse(response, Boolean.TRUE, " Se realizó la actualización exitosamente",
							null)
					: ParametrosUtil.setearResponse(response, Boolean.FALSE,
							" No se actualizó unos de los participantes evaluados"));
			LOGGER.info(Constantes.LOG_RESPONSE + JsonUtil.convertirObjetoACadenaJson(response));

			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	private Boolean actualizarParticipantes(NuevoEvaluadorServidorCivilEdicionDTO bean) {
		Boolean flagSave = Boolean.FALSE;
		// TODO: Evaluador
		Optional<DetUnidadOrganica> evaluador = detUnidadOrganicaRepository.findById(bean.getEvaluadorMMdetUoId());
		if (evaluador.isPresent()) {
			evaluador.get().setSegmentoId(Constantes.SEGMENTO_GDR_MANDO_MEDIO);
			evaluador.get().setCampoSegUpd(Constantes.ESTADO_ACTIVACION_USUARIO, bean.getUsuario(), Instant.now());
			if (evaluador.get().getRolId() == null) {
				evaluador.get().setRolId(Constantes.TIPO_ROL_EVALUADOR_Y_EVALUADO);
			} else {
				if (Constantes.TIPO_ROL_EVALUADO.equals(evaluador.get().getRolId())) {
					evaluador.get().setRolId(Constantes.TIPO_ROL_EVALUADOR_Y_EVALUADO);
				}
			}
			
			// TODO invocar al servicio gdr planificacion actualizar segmento y rol

			DetUnidadOrganica servCivil = new DetUnidadOrganica();
			servCivil = detUnidadOrganicaRepository.save(evaluador.get());
			LOGGER.info("[servCivil-Evaluador: ] " + JsonUtil.convertirObjetoACadenaJson(servCivil));

			flagSave = (servCivil != null && servCivil.getDetUnidadOrganicaId() != null) ? Boolean.TRUE : Boolean.FALSE;
		}

		// TODO: Evaluados
		for (NuevoEvaluadorDTO evaluado : bean.getEvaluados()) {
			Long detuoId = evaluado != null && evaluado.getDetuoId() != null ? evaluado.getDetuoId() : 0L;

			Optional<DetUnidadOrganica> detUO = detUnidadOrganicaRepository.findById(detuoId);
			if (detUO.isPresent()) {
				DetUnidadOrganica servCivil = new DetUnidadOrganica();
				detUO.get().setPersonaEvaluadorId(evaluador.get().getPersonaId());
				detUO.get().setCampoSegUpd(Constantes.ESTADO_ACTIVACION_USUARIO, bean.getUsuario(), Instant.now());
				servCivil = detUnidadOrganicaRepository.save(detUO.get());
				flagSave = Boolean.TRUE;
				LOGGER.info(Constantes.LOG_SERVCIVIL + JsonUtil.convertirObjetoACadenaJson(servCivil));
			} else {
				flagSave = Boolean.FALSE;
				LOGGER.info("[info: ya no se encuentra identificador unidad organica: " + detuoId + "]");
			}
		}
		return flagSave;
	}

	private Boolean actualizarParticipantesEvaluado(NuevoEvaluadorServidorCivilEdicionDTO bean) {
		Boolean flagSave = Boolean.FALSE;
		// TODO: Evaluador
		Optional<DetUnidadOrganica> evaluador = detUnidadOrganicaRepository.findById(bean.getEvaluadorMMdetUoId());
		if (evaluador.isPresent()) {
			//evaluador.get().setSegmentoId(Constantes.SEGMENTO_GDR_MANDO_MEDIO);
			/*evaluador.get().setCampoSegUpd(Constantes.ESTADO_ACTIVACION_USUARIO, bean.getUsuario(), Instant.now());
			if (evaluador.get().getRolId() == null) {
				evaluador.get().setRolId(Constantes.TIPO_ROL_EVALUADOR_Y_EVALUADO);
			} else {
				if (Constantes.TIPO_ROL_EVALUADO.equals(evaluador.get().getRolId())) {
					evaluador.get().setRolId(Constantes.TIPO_ROL_EVALUADOR_Y_EVALUADO);
				}
			}
			*/
			// TODO invocar al servicio gdr planificacion actualizar segmento y rol

			DetUnidadOrganica servCivil = new DetUnidadOrganica();
			servCivil = detUnidadOrganicaRepository.save(evaluador.get());
			LOGGER.info("[servCivil-Evaluador: ] " + JsonUtil.convertirObjetoACadenaJson(servCivil));

			flagSave = (servCivil != null && servCivil.getDetUnidadOrganicaId() != null) ? Boolean.TRUE : Boolean.FALSE;
		}

		// TODO: Evaluados
		for (NuevoEvaluadorDTO evaluado : bean.getEvaluados()) {
			Long detuoId = evaluado != null && evaluado.getDetuoId() != null ? evaluado.getDetuoId() : 0L;

			Optional<DetUnidadOrganica> detUO = detUnidadOrganicaRepository.findById(detuoId);
			if (detUO.isPresent()) {
				DetUnidadOrganica servCivil = new DetUnidadOrganica();
				detUO.get().setPersonaEvaluadorId(evaluador.get().getPersonaId());
				detUO.get().setCampoSegUpd(Constantes.ESTADO_ACTIVACION_USUARIO, bean.getUsuario(), Instant.now());
				servCivil = detUnidadOrganicaRepository.save(detUO.get());
				flagSave = Boolean.TRUE;
				LOGGER.info(Constantes.LOG_SERVCIVIL + JsonUtil.convertirObjetoACadenaJson(servCivil));
			} else {
				flagSave = Boolean.FALSE;
				LOGGER.info("[info: ya no se encuentra identificador unidad organica: " + detuoId + "]");
			}
		}
		return flagSave;
	}

	@Override
	public RespBase<Object> quitarEvaluadorAsignadoUnEvaluadoServidorCivil(Map<String, Object> parametroMap, MyJsonWebToken jwt)
			throws ValidationException {
		try {
			NuevoEvaluadorServidorCivilEdicionDTO bean = new NuevoEvaluadorServidorCivilEdicionDTO();
			RespBase<Object> response = new RespBase<>();
			Boolean flagSave = Boolean.FALSE;

			bean.setEntidadId((Long) parametroMap.get("entidadId"));
			bean.setUoId((Long) parametroMap.get("uoId"));
			bean.setEvaluadoDetUoId((Long) parametroMap.get("evaluadoDetUoId"));
			bean.setUsuario(jwt.getUsuario().getUsuario());
			LOGGER.info(Constantes.LOG_BEAN + JsonUtil.convertirObjetoACadenaJson(bean));

			Optional<DetUnidadOrganica> evaluado = detUnidadOrganicaRepository.findById(bean.getEvaluadoDetUoId());
			if (evaluado.isPresent()) {
				if (evaluado.get().getEntidadId().equals(bean.getEntidadId())
						&& evaluado.get().getOrganigramaId().equals(bean.getUoId())) {
					evaluado.get().setPersonaEvaluadorId(null);
					evaluado.get().setUsuarioModificacion(bean.getUsuario());
					evaluado.get().setFechaModificacion(Instant.now());

					DetUnidadOrganica servCivil = new DetUnidadOrganica();
					servCivil = detUnidadOrganicaRepository.save(evaluado.get());
					LOGGER.info("[servCivil-Evaluador: ] " + JsonUtil.convertirObjetoACadenaJson(servCivil));
				
					flagSave = (servCivil != null && servCivil.getDetUnidadOrganicaId() != null) ? Boolean.TRUE : Boolean.FALSE;
				}
			}

			response = (flagSave.equals(Boolean.TRUE)
					? ParametrosUtil.setearResponse(response, Boolean.TRUE,
							" Se retiró al evaluado de un evaluador exitosamente", null)
					: ParametrosUtil.setearResponse(response, Boolean.FALSE,
							" No se retiró al evaluado de un evaluador"));
			LOGGER.info(Constantes.LOG_RESPONSE + JsonUtil.convertirObjetoACadenaJson(response));

			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> editarServidorCivil(@Valid ReqBase<ReqEditaServidorCivil> request, MyJsonWebToken jwt)
			throws ValidationException {
		try {
			ServidorCivilEdicionDTO bean = new ServidorCivilEdicionDTO();
			RespBase<Object> response = new RespBase<>();
			String mensajeCorreo = "";
			String mensajeEmpleado = "";
			Map<String, Object> parametroData = null;

			bean.setDetalleuoId(request.getPayload().getDetuoId());
			bean.setEntidadId(request.getPayload().getEntidadId());
			bean.setPersonaId(request.getPayload().getPersonaId());
			bean.setTelefono(StringUtils.trimToEmpty(request.getPayload().getTelefono()));
			bean.setCorreoElectronicoInstitucional(
					StringUtils.trimToEmpty(request.getPayload().getCorreoInstitucional()));
			bean.setCorreoElectronicoAlternativo(StringUtils.trimToEmpty(request.getPayload().getCorreoAlterno()));
			bean.setSindicatoFlag(StringUtils.trimToEmpty(request.getPayload().getSindicatoFlag()));
			bean.setUsuario(jwt.getUsuario().getUsuario());
			bean.setLongPersonaId(Long.valueOf(bean.getPersonaId()));
			bean.setFoto(request.getPayload().getFoto());
			bean.setRegimenLaboralId(request.getPayload().getRegimenLaboralId());

			parametroData = actualizarCorreo(bean, jwt);
			LOGGER.info("[parametro: ] " + JsonUtil.convertirObjetoACadenaJson(parametroData));
			mensajeCorreo = (String) parametroData.get(Constantes.MENSAJE);

			mensajeEmpleado = actualizarDatoServidorCivil(bean, response, mensajeEmpleado);

			response = validarMensajeCorreo(response, mensajeCorreo, mensajeEmpleado);
			LOGGER.info(Constantes.LOG_RESPONSE + JsonUtil.convertirObjetoACadenaJson(response));

			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	private RespBase<Object> validarMensajeCorreo(RespBase<Object> response, String mensajeCorreo,
			String mensajeEmpleado) {
		if (mensajeCorreo != null && mensajeCorreo.isEmpty()) {
			if (mensajeEmpleado != null && mensajeEmpleado.isEmpty()) {
				response = ParametrosUtil.setearResponse(response, Boolean.TRUE,
						" Se realizó la actualización exitosamente", null);
			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, mensajeEmpleado);
			}
		} else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, mensajeCorreo);
		}
		return response;
	}

	private String actualizarDatoServidorCivil(ServidorCivilEdicionDTO bean, RespBase<Object> response,
			String mensajeEmpleado) {
		Map<String, Object> parametroData;

		// Actualizar telefono
		if (!bean.getTelefono().isEmpty()) {

			String regexpNumero = "[0-9]*";
			if (Pattern.matches(regexpNumero, bean.getTelefono())) {
				parametroData = registrarTelefono(bean);
				LOGGER.info("[parametor: ]" + JsonUtil.convertirObjetoACadenaJson(parametroData));
			} else {
				return mensajeEmpleado = "El campo teléfono solo debe contener números";
			}

		}

		Long detuoId = bean.getDetalleuoId();
		if (detuoId == null)
			detuoId = 0L;

		Optional<DetUnidadOrganica> detUO = detUnidadOrganicaRepository.findById(detuoId);

		DetUnidadOrganica servCivil = null;
		Empleado empleado = null;

		if (detUO.isPresent()) {
			// Servicio para actualizar datos personales en servidor civil: correo
			// institucional y sindicato
			empleado = empleadoRepository.findByEntidadIdAndPersonaIdAndPuestoIdAndEstadoRegistro(detUO.get().getEntidadId(),
					detUO.get().getPersonaId(), detUO.get().getPuestoId(), detUO.get().getEstadoRegistro());
			if (empleado != null) {
				// Actualizar sindicato
				empleado.setRegimenLaboral(
						bean.getRegimenLaboralId() != null ? bean.getRegimenLaboralId() : empleado.getRegimenLaboral());
				empleado.setSindicatoFlag(bean.getSindicatoFlag());
				empleado.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), bean.getUsuario(), Instant.now());

				empleadoRepository.save(empleado);
			} else {
				mensajeEmpleado = "NO SE ENCONTRO EMPLEADO EN EL SERVIDOR CIVIL";
			}

			detUO.get().setUsuarioModificacion(bean.getUsuario());
			detUO.get().setFechaModificacion(Instant.now());

			servCivil = detUnidadOrganicaRepository.save(detUO.get());
			response.getStatus().setSuccess(Boolean.TRUE);
			response.setPayload(servCivil);
		} else {
			// Servicio para actualizar perfile de usuario: telefono, correo alterno,
			// sindicato y foto
			empleado = empleadoRepository.findByEntidadIdAndPersonaId(bean.getEntidadId(), bean.getLongPersonaId());
			if (empleado != null) {
				/****/
				if (bean.getFoto() != null) {
					// Actualizar foto
					parametroData = actualizarFoto(bean.getFoto(), empleado);
					if (!parametroData.isEmpty()) {
						mensajeEmpleado = (String) parametroData.get(Constantes.MENSAJE);
						Integer flag = (Integer) parametroData.get("flag");
						bean.getFoto().setFlag(flag);

						if (bean.getFoto().getFlag() == 1) {
							// Actualizar url foto
							empleado.setUrlFoto((String) parametroData.get("rutaFoto"));
						}
					}
				}

				// Actualizar sindicato
				empleado.setSindicatoFlag(bean.getSindicatoFlag());
				empleado.setRegimenLaboral(
						bean.getRegimenLaboralId() != null ? bean.getRegimenLaboralId() : empleado.getRegimenLaboral());

				empleado.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), bean.getUsuario(), Instant.now());
				empleado.setEntidadId(bean.getEntidadId());
				empleado.setPersonaId(bean.getLongPersonaId());

				empleadoRepository.save(empleado);
				/****/

			} else {
				mensajeEmpleado = " NO SE ENCONTRO EMPLEADO EN EL SERVIDOR CIVIL";
			}
		}
		return mensajeEmpleado;
	}

	@Override
	public RespBase<Object> validarListaServCivil(MyJsonWebToken token, List<ServidorCivilExcelDTO> lista,
			Long entidadId) throws Exception {
		RespBase<Object> responseMasivo = new RespBase<>();
		Parametro limiteRegistro = generalRepository.buscarParametro(null, Constantes.LIMIT_MASIVO, null);

		ReqBase<BeanServidorCivilDTO> servidorCivilReq = new ReqBase<>();
		BeanServidorCivilDTO beanServidorCivilDTO = new BeanServidorCivilDTO();
		beanServidorCivilDTO.setEntidadId(entidadId);

		responseMasivo = verificaListaServCivil(token, lista, responseMasivo, limiteRegistro, servidorCivilReq, beanServidorCivilDTO);

		if(!responseMasivo.getStatus().getSuccess()) {
			responseMasivo.getStatus().setSuccess(Boolean.FALSE);
		}
		
		return responseMasivo;
	}

	private RespBase<Object> verificaListaServCivil(MyJsonWebToken token, List<ServidorCivilExcelDTO> lista,
			RespBase<Object> responseMasivo, Parametro limiteRegistro, ReqBase<BeanServidorCivilDTO> servidorCivilReq,
			BeanServidorCivilDTO beanServidorCivilDTO) throws Exception {
		RespBase<GenericResponseMessage> response = new RespBase<>();
		
		lista.removeIf(x -> !StringUtils.isEmpty(x.getEliminarFila()) && x.isFlagRegistrar() == false);
		if (limiteRegistro.getCodigoNumero() >= lista.size()) {

			List<Organigrama> lstOrganigrama = organigramaRepository.findByEntidadId(beanServidorCivilDTO.getEntidadId());
	
			if (!CollectionUtils.isEmpty(lstOrganigrama)) {

				boolean existeObs = false;
				for (ServidorCivilExcelDTO civExcelDTO : lista) {
					LOGGER.info(">>>>>>>>> VALIDANDO registro SC : " + civExcelDTO.toString());
					
					String obs = civExcelDTO.getObservacion();
					
					boolean isOKPuestoEnUO = false;
					
					if (StringUtils.isEmpty(obs)) {
						isOKPuestoEnUO = validarPuestoEnUO(beanServidorCivilDTO.getEntidadId(), civExcelDTO, lista);
						if (!isOKPuestoEnUO) {
							existeObs = true;
						}
						
					} else {
						existeObs = true;
					}
		
					if (StringUtils.isEmpty(obs) && civExcelDTO.isFlagRegistrar() && isOKPuestoEnUO) {
		
						Long organoId = Long.parseLong(civExcelDTO.getSiglaId().trim());
						Long regimenLaboralId = Long.parseLong(civExcelDTO.getRegimenId().trim());
						
						if (!lstOrganigrama.stream().anyMatch(o -> o.getOrganigramaId().equals(organoId))) {
							existeObs = true;
							civExcelDTO.agregarObservacion("El código del Organo / UO no esta registrado previamente");
						}
						
						long nroRepetidasDoc = lista.stream().filter(s -> s.getNumeroDocumento() != null && s.getNumeroDocumento().equals(civExcelDTO.getNumeroDocumento())).count();
						if (nroRepetidasDoc > 1) {
							existeObs = true;
							civExcelDTO.agregarObservacion("El número de documento se repite más de una vez en el archivo");
						}
						
						if (regimenLaboralId.equals(0L)) {
							existeObs = true;
							civExcelDTO.agregarObservacion("El código ID del Regimen Laboral no ha sido encontrado");
						}
					}
					
					if (StringUtils.isEmpty(obs)) {
						boolean existeObsValidacion = ejecutarValidacionCrearServidorCivil(civExcelDTO);
						if (existeObsValidacion) {
							existeObs = true;
						}
					}
				}
				
				if (!existeObs) {

					boolean huboProblemasCrearSC = false;
					for (ServidorCivilExcelDTO civExcelDTO : lista) {
						LOGGER.info(">>>>>>>>> GRABANDO registro SC : " + civExcelDTO.toString());

						Long organoId = Long.parseLong(civExcelDTO.getSiglaId().trim());
						Long regimenLaboralId = Long.parseLong(civExcelDTO.getRegimenId().trim());
						String sindicatoId = civExcelDTO.getSindicato().equals(Constantes.SELECCIONAR) ? "" : civExcelDTO.getSindicatoId().trim();
						
						ServidorCivilGDRDTO civGDRDTO = new ServidorCivilGDRDTO();
						
						civGDRDTO.setTipoDocumento(Integer.parseInt(civExcelDTO.getDocumentoId().trim()));
						civGDRDTO.setNumeroDocumento(civExcelDTO.getNumeroDocumento());
						civGDRDTO.setApellidoPaterno(civExcelDTO.getApellidoPaterno().toUpperCase());
						civGDRDTO.setApellidoMaterno(civExcelDTO.getApellidoMaterno().toUpperCase());
						civGDRDTO.setNombres(civExcelDTO.getNombres().toUpperCase());
						civGDRDTO.setCorreoElectronico(civExcelDTO.getCorreoLaboral().toLowerCase().trim());
						civGDRDTO.setSexo(civExcelDTO.getSexoId().trim());
						civGDRDTO.setPuestoDescripcion(civExcelDTO.getPuesto().toUpperCase());
						civGDRDTO.setPuestoId(Long.parseLong(civExcelDTO.getPuestoId().trim()));
						civGDRDTO.setFechaNacimiento(ParametrosUtil.StringToLocalDateDMY(civExcelDTO.getFechaNacimiento()));
						civGDRDTO.setFechaInicio(ParametrosUtil.StringToLocalDateDMY(civExcelDTO.getFechaInicio()));
						civGDRDTO.setOrganoId(organoId);
						civGDRDTO.setRegimenLaboralId(regimenLaboralId);
						civGDRDTO.setSindicatoId(sindicatoId);
						civGDRDTO.setResponsable(civExcelDTO.getResponsable().equalsIgnoreCase(Constantes.SI) ? "S" : "N");
						civGDRDTO.setTipoAsignacion(Constantes.TIPO_ASIGNACION_PRINCIPAL);

						beanServidorCivilDTO.setServidorCivil(civGDRDTO);
						servidorCivilReq.setPayload(beanServidorCivilDTO);
						
						try {
							response = crearServidorCivil(servidorCivilReq, token);
							
							if (Boolean.FALSE.equals(response.getStatus().getSuccess()) ) {
								huboProblemasCrearSC = true;
								String mensajeResultado = "";
								for (String mensaje : response.getStatus().getError().getMessages()) {
									mensajeResultado = mensajeResultado + mensaje + ",";
								}
								mensajeResultado = mensajeResultado.trim().substring(0, mensajeResultado.length() - 1);
//								civExcelDTO.setObservacionResultado(mensajeResultado);
								civExcelDTO.agregarObservacion(mensajeResultado);
							}
						} catch (Exception e) {
							System.out.println("<ERROR> Al ejecutar el metodo del servicio crearServidorCivil.");
							System.out.println(e.getMessage());
							huboProblemasCrearSC = true;
							civExcelDTO.agregarObservacion(e.getMessage());
						}

					}
					
					if (huboProblemasCrearSC) {
						responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE, "Hubo problemas al crear Servidor Civil en alguna fila del archivo");
						responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacionResultado().isEmpty()));
					} else {
						responseMasivo.setPayload(response.getPayload().getClass());
						responseMasivo.getStatus().setSuccess(Boolean.TRUE);
					}
					
				} else {
					LOGGER.info(">>>>>>>>> El archivo TIENE ERRORES, NO GRABARA REGISTROS DE SC ");
					responseMasivo.setPayload(lista.stream().filter(mc -> !mc.getObservacion().isEmpty()));
					responseMasivo.getStatus().setSuccess(Boolean.FALSE);
				}

			} else {
				responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
						"No existen Organos / Unidades Organicas registrados para esta entidad. ");
			}

		} else {
			responseMasivo = ParametrosUtil.setearResponse(responseMasivo, Boolean.FALSE,
					Constantes.MENSAJE_LIMITE_MASIVO + limiteRegistro.getCodigoNumero());
		}

		return responseMasivo;
	}

	private boolean validarPuestoEnUO(Long entidadId, ServidorCivilExcelDTO servidorCivilExcelDTO, List<ServidorCivilExcelDTO> lista) {

		boolean pasoValidacion = true;
		
		Long organigramaId = Long.valueOf(servidorCivilExcelDTO.getSiglaId());

		Optional<Puesto> valPuesto = puestoRepository.findByEntidadIdAndDescripcionAndOrganigramaIdAndEstadoRegistro(
				entidadId, servidorCivilExcelDTO.getPuesto().toUpperCase(), organigramaId, Constantes.ESTADO_ACTIVO);

		if (valPuesto.isPresent()) {
			String esJefe = valPuesto.get().getEsJefe();
			if (esJefe.equals(Constantes.ES_RESPONSABLE_S)) {
				
				if (lista.stream()
						.anyMatch(sc -> (sc.getNumeroDocumento() != null
								&& !sc.getNumeroDocumento().equals(servidorCivilExcelDTO.getNumeroDocumento()))
								&& ((sc.getPuesto() != null
										&& sc.getPuesto().equalsIgnoreCase(servidorCivilExcelDTO.getPuesto()))
										|| (sc.getPuestoId() != null
												&& sc.getPuestoId().equals(servidorCivilExcelDTO.getPuestoId()))))) {
					pasoValidacion = false;
					servidorCivilExcelDTO.agregarObservacion("Ya existe un servidor civil asignado para el puesto seleccionado en el mismo archivo");
				}				
				
				Optional<Organigrama> oOrganigrama = organigramaRepository.findById(organigramaId);
				if (oOrganigrama.isPresent()) {
					if (oOrganigrama.get().getPersonaResponsableId() != null) {
						pasoValidacion = false;
						servidorCivilExcelDTO.agregarObservacion("Ya existe un servidor civil asignado para el puesto seleccionado");
					}
				}
//				servidorCivilExcelDTO.setResponsable(Constantes.ES_RESPONSABLE_S);
			}
		} else {
			pasoValidacion = false;
			servidorCivilExcelDTO.agregarObservacion("El puesto NO pertenece a la UO de la entidad");
		}

		return pasoValidacion;
	}
	
	private boolean ejecutarValidacionCrearServidorCivil(ServidorCivilExcelDTO civExcelDTO) {
		boolean existeObs = false;
		
		@SuppressWarnings("rawtypes")
		RespBase<ApiPersonaRequestDTO> request = beanAdapter.adapToBeanPersona(civExcelDTO);
		
		try {
			RespBase<String> responseValidarPersona = personaApiClient.validarPersonaNatural(request);

			if (Boolean.FALSE.equals(responseValidarPersona.getStatus().getSuccess())) {
				existeObs = true;
				civExcelDTO.agregarObservacion("Hubo un error en API Persona al validar Persona Natural");
			}

		} catch (FeignException e) {
			
			existeObs = true;
			if (e.status() == Integer.parseInt(Constantes.SERVER_400)) {
				String error = e.getMessage().substring(e.getMessage().indexOf("{"), e.getMessage().length() - 1);
				try {
					@SuppressWarnings("unchecked")
					RespBase<Object> respo = JsonUtil.convertirCadenaJsonPostAObjeto(error, RespBase.class);
					for (String msg : respo.getStatus().getError().getMessages()) {
						civExcelDTO.agregarObservacion(msg);
					}
					
				} catch (Exception e1) {
					System.out.println(e1.getMessage());
				}
			} else {
				civExcelDTO.agregarObservacion("Ocurrio un error al invocar el servicio del API Persona (validarPersonaNatural). Message =".concat(e.getMessage()));
			}
		}
				
		return existeObs;
	}
	
	@Override
	public List<ServidorCivilExcelDTO> obtenerListaServCivilfromExcel(InputStream uploadedInputStream) {
		try {
			ExcelUtil<ServidorCivilExcelDTO> file = new ExcelUtil<>(ServidorCivilExcelDTO::new);

			int[] hojasLeer = { Constantes.HOJA_EXCEL_CERO, Constantes.HOJA_EXCEL_DOS };
			List<ServidorCivilExcelDTO> listaServCivil = file.utilExcelToPojo(uploadedInputStream, hojasLeer);

			return listaServCivil;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);

		}
		return null;// NOSONAR
	}

	@Override
	public RespBase<Object> agregarPuesto(@Valid ReqBase<ReqAgregarPuesto> request, MyJsonWebToken jwt)
			throws ValidationException {
		try {

			RespBase<Object> responseServidores = new RespBase<>();
			PuestoAgregarDTO bean = new PuestoAgregarDTO();
			RespBase<Object> response = new RespBase<>();

			bean.setUoId(request.getPayload().getUoId());
			bean.setPersonaId(request.getPayload().getPersonaId());
			bean.setEntidadId(request.getPayload().getEntidadId());
			bean.setPuestoId(request.getPayload().getPuestoId());
			bean.setPuesto(request.getPayload().getPuesto());
			bean.setFechaInicio(request.getPayload().getFechaInicio());
			bean.setTipoAsignacion(request.getPayload().getTipoAsignacion());
			bean.setPersonaIdAsignada(request.getPayload().getPersonaIdAsignada());
			LOGGER.info(Constantes.LOG_BEAN + JsonUtil.convertirObjetoACadenaJson(bean));

			// Validaciones antes de realizar las transacciones
			Map<String, Object> parametro = new HashedMap<>();
			parametro = beanAdapter.adapToParamterValidacionAgregarPuesto(bean);
			String estado = (String) parametro.get(Constantes.ESTADO);
			String mensaje = (String) parametro.get(Constantes.MENSAJE);
			if (estado.equals(Constantes.ESTADO_NO_ES_VALIDO)) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, mensaje);
				responseServidores.setPayload(response);
				LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(responseServidores));
				return responseServidores;
			}

			Long puestoId = null;

			Optional<Puesto> puesto = puestoRepository.findById(bean.getPuestoId());
			if (!puesto.isPresent()) {
				// INGRESAR PUESTO NUEVO
				Puesto puestoInsert = new Puesto();
				puestoInsert.setEntidadId(bean.getEntidadId());
				puestoInsert.setDescripcion(bean.getPuesto());
				puestoInsert.setCampoSegIns(jwt.getUsuario().getUsuario(), Instant.now());
				puestoInsert = puestoRepository.save(puestoInsert);
				puestoId = puestoInsert.getPuestoId();
				LOGGER.info("[puestoInsert: ] " + JsonUtil.convertirObjetoACadenaJson(puestoInsert));
			} else {
			puestoId = puesto.get().getPuestoId();
			}
			///Actualizar puesto

			// Validar unidad organica
			Optional<UnidadOrganica> unidadOrganica = unidadOrganicaRepository.findById(bean.getUoId());
			if (unidadOrganica.isPresent()) {

				DetUnidadOrganica detUO = new DetUnidadOrganica();

				if (bean.getTipoAsignacion().equals(Constantes.TIPO_ASIGNACION_ENCARGATURA)) {
					if (bean.getPersonaIdAsignada() != null) {
						// detUO.setTipoAsignacion(bean.getTipoAsignacion());
					}
				}

				detUO.setOrganigramaId(bean.getUoId());
				detUO.setEntidadId(bean.getEntidadId());
				detUO.setPersonaId(bean.getPersonaId());
				detUO.setPuestoId(puestoId);
				detUO.setResponsable(Constantes.ES_RESPONSABLE_N);
				detUO.setExcluye(Constantes.CARACTER_VACIO);
				detUO.setTipoAsignacion(bean.getTipoAsignacion());
				detUO.setFechaInicioPuesto(bean.getFechaInicio());
				detUO.setEstadoRegistro(Constantes.ESTADO_ACTIVO);
				detUO.setUsuarioCreacion(jwt.getUsuario().getUsuario());
				detUO.setFechaCreacion(Instant.now());
				LOGGER.info("[detUO: ] " + JsonUtil.convertirObjetoACadenaJson(detUO));

				// Actualizar detalle unidad organica
				DetUnidadOrganica servCivil = new DetUnidadOrganica();
				servCivil = detUnidadOrganicaRepository.save(detUO);

				// response.getStatus().setSuccess(Boolean.TRUE);
				response = ParametrosUtil.setearResponse(response, Boolean.TRUE, 
						" Se realizó la actualización historial de puestos en el servidor civil exitosamente", null);
				response.setPayload(servCivil);

				LOGGER.info(Constantes.LOG_RESPONSE + JsonUtil.convertirObjetoACadenaJson(response));
			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, " NO SE ENCONTRO LA UNIDAD ORGANICA EN EL SERVIDOR CIVIL");
			}

			LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(responseServidores));

			if (Boolean.TRUE.equals(response.getStatus().getSuccess()) && bean.getPersonaIdAsignada() != null) {
				ReqBase<DetUnidadOrganica> requestDetUnOrga = beanAdapter.adapToRequestDetUnidad(bean.getPersonaIdAsignada(), request.getPayload().getFechaInicio());
				cesarServidorCivil(requestDetUnOrga, jwt);
			}

			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> actualizarDetallePuesto(@Valid ReqBase<ReqActualizarPuesto> request, MyJsonWebToken jwt)
			throws ValidationException {
		try {

			PuestoActualizarDTO bean = new PuestoActualizarDTO();
			RespBase<Object> response = new RespBase<>();
			RespBase<Object> responseServidores = new RespBase<>();
			BeanAdapterServidorCivil adaptador = new BeanAdapterServidorCivil();
			bean.setDetuoId(request.getPayload().getDetuoId());
			bean.setEntidadId(request.getPayload().getEntidadId());
			bean.setFechaCese(request.getPayload().getFechaCese());
			bean.setStrFechaInicio(request.getPayload().getStrFechaInicio());
			bean.setNombrePuesto(request.getPayload().getNombrePuesto());
			bean.setPersonaIdAsignada(request.getPayload().getPersonaIdAsignada());
			bean.setPuestoId(request.getPayload().getPuestoId());
			bean.setTipoAsignacion(request.getPayload().getTipoAsignacion());
			bean.setUoId(request.getPayload().getUoId());
			bean.setUsuario(jwt.getUsuario().getUsuario());
			Boolean flagValida = Boolean.FALSE;
			if (bean.getStrFechaInicio() != null) {
				Date fechaInicio = null;
				adaptador = new BeanAdapterServidorCivil();
				fechaInicio = adaptador.adapToDate(bean.getStrFechaInicio());
				if (fechaInicio != null) {
					bean.setFechaInicio(fechaInicio);
				} else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "DEBE INGRESAR FECHA DE INICIO VALIDO EN EL SERVIDOR CIVIL!");
					responseServidores.setPayload(response);
					LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(responseServidores));
					flagValida = Boolean.TRUE;
				}
			} else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "DEBE INGRESAR FECHA DE INICIO EN EL SERVIDOR CIVIL!");
				responseServidores.setPayload(response);
				LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(responseServidores));
				flagValida = Boolean.TRUE;
			}
			
			if (bean.getStrFechaCese() != null) {
				Date fechaCese = null;
				adaptador = new BeanAdapterServidorCivil();
				fechaCese = adaptador.adapToDate(bean.getStrFechaCese());
				if (fechaCese != null) {
					bean.setFechaInicio(fechaCese);
				} else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "DEBE INGRESAR FECHA DE CESE VALIDO EN EL SERVIDOR CIVIL!");
					responseServidores.setPayload(response);
					LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(responseServidores));
					flagValida = Boolean.TRUE;
				}
			}

			LOGGER.info(Constantes.LOG_BEAN + JsonUtil.convertirObjetoACadenaJson(bean));

			// Validaciones antes de realizar las transacciones
			Map<String, Object> parametro = new HashedMap<>();
			parametro = beanAdapter.adapToParamterValidacionActualizarDetllePuesto(bean);
			String estado = (String) parametro.get(Constantes.ESTADO);
			String mensaje = (String) parametro.get(Constantes.MENSAJE);
			if (estado.equals(Constantes.ESTADO_NO_ES_VALIDO)) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, mensaje);
				responseServidores.setPayload(response);
				LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(responseServidores));
				flagValida = Boolean.TRUE;
			}
			
			if (flagValida) return responseServidores;

			Map<String, Object> param = new HashedMap<>();

			if (bean.getPuestoId() != null && bean.getNombrePuesto() == null) {// ready
				LOGGER.info("Si puestoId es <> nulo y nombrePuesto nulo, update detalle_uo el puestoId");
				param = actualizarDetalleUnidadOrganica(bean);
				Boolean estadoParam = (boolean) param.get(Constantes.ESTADO);
				DetUnidadOrganica servCivil = (DetUnidadOrganica) param.get(Constantes.OBJDETALLEUO);
				String mensajeParam = (String) param.get(Constantes.MENSAJE);
				int cantidad = actualizarEmpleado(bean );
				if(cantidad == 1) {
					LOGGER.info(
							"Se actualizó el puesto en Empleado");
				}

				if (estadoParam) {
					response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam, null);
					response.setPayload(servCivil);
				} else {
					response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam);
				}
			}

			if (bean.getPuestoId() != null && bean.getNombrePuesto() != null) {// ready
				LOGGER.info(
						"Si puestoId es <> nulo y nombrePuesto <> nulo, update detalle_uo el puestoId y actualiza nombre de puesto en la tabla puesto");
				param = actualizarDetalleUnidadOrganica(bean);
				Boolean estadoParam = (boolean) param.get(Constantes.ESTADO);
				DetUnidadOrganica servCivil = (DetUnidadOrganica) param.get(Constantes.OBJDETALLEUO);
				String mensajeParam = (String) param.get(Constantes.MENSAJE);
				LOGGER.info(Constantes.LOG_SERVCIVIL + JsonUtil.convertirObjetoACadenaJson(servCivil));

				if (estadoParam) {
					param = registrartPuesto(bean);					
					estadoParam = (boolean) param.get(Constantes.ESTADO);
					Puesto puestoServCivil = (Puesto) param.get(Constantes.OBJPUESTO);
					mensajeParam = (String) param.get(Constantes.MENSAJE);
					
					if (estadoParam) {
						response.setPayload(puestoServCivil);
						response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam, null);
					} else {
						response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam);
					}
				} else {
					response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam);
				}
			}

			if (bean.getPuestoId() == null && bean.getNombrePuesto() != null) {// ready
				LOGGER.info(
						"Si puestoId es = nulo y nombrePuesto <> nulo,  insertar nombre puesto en la tabla puesto y actualiza detalle_uo el puestoId");
				param = registrartPuesto(bean);
				Boolean estadoParam = (boolean) param.get(Constantes.ESTADO);
				Puesto puestoServCivil = (Puesto) param.get(Constantes.OBJPUESTO);
				String mensajeParam = (String) param.get(Constantes.MENSAJE);
				LOGGER.info("[puestoServCivil: ] " + JsonUtil.convertirObjetoACadenaJson(puestoServCivil));
			
				if (estadoParam) {
					param = actualizarDetalleUnidadOrganica(bean);
					estadoParam = (boolean) param.get(Constantes.ESTADO);
					DetUnidadOrganica servCivil = (DetUnidadOrganica) param.get(Constantes.OBJDETALLEUO);
					mensajeParam = (String) param.get(Constantes.MENSAJE);
					LOGGER.info(Constantes.LOG_SERVCIVIL + JsonUtil.convertirObjetoACadenaJson(servCivil));
					response.setPayload(servCivil);

					if (estadoParam) {
						response.setPayload(puestoServCivil);
						response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam, null);
					} else {
						response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam);
					}
				} else {
					response = ParametrosUtil.setearResponse(response, estadoParam, mensajeParam);
				}
			}

			if (bean.getPuestoId() == null && bean.getNombrePuesto() == null) {// ready
				LOGGER.info("puestoId es = nulo y nombrePuesto = nulo, validar error");
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						" SE REQUIERE INGRESAR UN PUESTO VALIDO");
			}

			if (bean.getPersonaIdAsignada() != null
					&& Boolean.TRUE.equals(response.getStatus().getSuccess())) {
				ReqBase<DetUnidadOrganica> requestDetUnOrga = beanAdapter.adapToRequestDetUnidad(
						bean.getPersonaIdAsignada(), 
						bean.getFechaInicio() );
				cesarServidorCivil(requestDetUnOrga, jwt);
			}
			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	private int actualizarEmpleado(PuestoActualizarDTO bean	) {
		Empleado emp = new Empleado();
		emp.setEntidadId(bean.getEntidadId());
		emp.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
		emp.setPersonaId(bean.getPersonaIdAsignada());
//		emp.setPuestoId(bean.getPuestoId());
		Example<Empleado> Oemp = Example.of(emp);
		List<Empleado> listEmpleados = empleadoRepository.findAll(Oemp);
		if(listEmpleados.size()>0) {			
			Empleado upEmp = listEmpleados.get(0);
			upEmp.setPuestoId(bean.getPuestoId());
			empleadoRepository.save(upEmp);
			return 1;
					
		};
		return 0;
	}

	private Map<String, Object> registrartPuesto(PuestoActualizarDTO bean) {

		Map<String, Object> parametro = new HashedMap<>();
		String mensaje = "";

		try {
			Optional<Puesto> puesto = puestoRepository.findByDescripcion(bean.getNombrePuesto());
			if (puesto.isPresent()) {
				// ACTUALIZAR PUESTO EXISTENTE
				Puesto puestoUpdate = new Puesto();
				puesto.get().setEntidadId(bean.getEntidadId());
				puesto.get().setDescripcion(bean.getNombrePuesto());
				puesto.get().setCampoSegUpd(Constantes.ESTADO_ACTIVO, bean.getUsuario(), Instant.now());
				LOGGER.info("[puesto.get(): ] " + JsonUtil.convertirObjetoACadenaJson(puesto.get()));

				puestoUpdate = puestoRepository.save(puesto.get());
				mensaje = "Se realizó la actualización del puesto exitosamente";
				parametro.put(Constantes.OBJPUESTO, puestoUpdate);
				parametro.put(Constantes.ESTADO, Boolean.TRUE);
			} else {
				// INGRESAR PUESTO NUEVO
				Puesto puestoInsert = new Puesto();
				puestoInsert.setEntidadId(bean.getEntidadId());
				puestoInsert.setDescripcion(bean.getNombrePuesto());
				puestoInsert.setCampoSegIns(bean.getUsuario(), Instant.now());
				LOGGER.info("[puestoInsert: ] " + JsonUtil.convertirObjetoACadenaJson(puestoInsert));

				puestoInsert = puestoRepository.save(puestoInsert);
				mensaje = "Se realizó la insercción del puesto exitosamente";
				parametro.put(Constantes.OBJPUESTO, puestoInsert);
				parametro.put(Constantes.ESTADO, Boolean.TRUE);
			}
		} catch (Exception e) {
			mensaje = "Error en la insercción del puesto";
			parametro.put(Constantes.OBJPUESTO, null);
			parametro.put(Constantes.ESTADO, Boolean.FALSE);
		}

		parametro.put(Constantes.MENSAJE, mensaje);
		return parametro;
	}
	
	/********************************************************************************************************************
	 * 
	 * METODOS PRIVADOS
	 * 
	 * @param jwt
	 *
	 ********************************************************************************************************************/
	// Actualizar correo
	private Map<String, Object> actualizarCorreo(ServidorCivilEdicionDTO bean, MyJsonWebToken jwt) {

		RespBase<RespObtenerCorreo> responseCorreos = null;
		Map<String, Object> parametro = new HashedMap<>();
		String mensaje = "";

		responseCorreos = personaApiClient.obtenerCorreoBypersonaId(bean.getPersonaId());
		mensaje = validarCorreo(bean, responseCorreos, mensaje, jwt);

		parametro.put(Constantes.ESTADO, (mensaje != null && !mensaje.trim().isEmpty()) ? Boolean.FALSE : Boolean.TRUE);
		parametro.put(Constantes.MENSAJE, mensaje);

		return parametro;
	}

	private String validarCorreo(ServidorCivilEdicionDTO bean, RespBase<RespObtenerCorreo> responseCorreos,
			String mensaje, MyJsonWebToken jwt) {
		if (!CollectionUtils.isEmpty(responseCorreos.getPayload().getItems())) {
			// Validar se envie un tipo de correo
			if (!Constantes.VACIO.equals(StringUtils.trimToEmpty(bean.getCorreoElectronicoInstitucional()))
					&& !Constantes.VACIO.equals(StringUtils.trimToEmpty(bean.getCorreoElectronicoAlternativo()))) {
				mensaje = " Seleccione un tipo de correo Institucional o Alternativo y no los dos tipos!";
			} else {
				// Actualizar correos

				String correo = (Constantes.VACIO)
						.equals(StringUtils.trimToEmpty(bean.getCorreoElectronicoInstitucional()))
								? bean.getCorreoElectronicoAlternativo()
								: bean.getCorreoElectronicoInstitucional();
				List<Correo> listarCorreoRepetido = correoRepository.findByCorreo(StringUtils.trimToEmpty(correo));
				Long contarCorreo = listarCorreoRepetido.stream().filter(c -> c.getCorreo().equals(correo)).count();

				if (contarCorreo <= 1L) {
					if (!StringUtils.trimToEmpty(bean.getCorreoElectronicoInstitucional()).equals(Constantes.VACIO)) {
						Optional<Correo> correoPrincipal = correoRepository
								.findBypersonaIdAndTipoCorreo(bean.getPersonaId(), Constantes.TIPO_CORREO_PRINC);
						if (correoPrincipal.isPresent()) {
							correoPrincipal.get()
									.setCorreo(StringUtils.trimToEmpty(bean.getCorreoElectronicoInstitucional()));
							correoPrincipal.get().setTipoCorreo(Constantes.TIPO_CORREO_PRINC);
							correoPrincipal.get().setCampoSegUpd(Constantes.ESTADO_ACTIVO, bean.getUsuario(),
									Instant.now());
							correoRepository.save(correoPrincipal.get());

							Optional<Persona> persona = personaRepository.findById(bean.getLongPersonaId());
							if (persona.isPresent()) {
								persona.get().setCorreoId(correoPrincipal.get().getCorreoId());
								persona.get().setUsuarioModificacion(bean.getUsuario());
								persona.get().setFechaModificacion(Instant.now());
								// Actualizar correo de la persona
								Persona personaUpdate = personaRepository.save(persona.get());
								LOGGER.info(Constantes.LOG_RESPONSESERVIDORES
										+ JsonUtil.convertirObjetoACadenaJson(personaUpdate));
							} else {
								mensaje = Constantes.MSG_NO_SE_ENCONTRO_LA_PERSONA_EN_EL_SERVIDOR_CIVIL;
							}
						}
					}
					if (!StringUtils.trimToEmpty(bean.getCorreoElectronicoAlternativo()).equals(Constantes.VACIO)
							&& StringUtils.trimToEmpty(bean.getCorreoElectronicoInstitucional())
									.equals(Constantes.VACIO)) {

						List<Correo> correoAlternativo = correoRepository.findBypersonaIdAndTipoCorreoAlternativo(
								bean.getPersonaId(), Constantes.TIPO_CORREO_ALTER);

						if (CollectionUtils.isEmpty(correoAlternativo)) {
							Correo reistrarCorreo = new Correo();
							reistrarCorreo.setCorreo(StringUtils.trimToEmpty(bean.getCorreoElectronicoAlternativo()));
							reistrarCorreo.setPersonaId(bean.getLongPersonaId());
							reistrarCorreo.setTipoCorreo(Constantes.TIPO_CORREO_ALTER);
							reistrarCorreo.setCampoSegIns(jwt.getUsuario().getUsuario(), Instant.now());
							correoRepository.save(reistrarCorreo);
						} else {
							for (Correo update : correoAlternativo) {
								update.setCorreo(StringUtils.trimToEmpty(bean.getCorreoElectronicoAlternativo()));
								update.setTipoCorreo(Constantes.TIPO_CORREO_ALTER);
								update.setCampoSegUpd(Constantes.ESTADO_ACTIVO, bean.getUsuario(), Instant.now());
								correoRepository.save(update);
							}
						}
					}
				} else {
					mensaje = " Dirección de correo ya existe, por favor intente con uno distinto";
				}
			}
		} else {
			mensaje = " NO SE ENCONTRO CORREO DE LA PERSONA EN EL SERVIDOR CIVIL";
		}

		return mensaje;
	}

	// Actualizar telefono
	private Map<String, Object> registrarTelefono(ServidorCivilEdicionDTO bean) {
		RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = null;
		RespBase<RespObtenerTelefono> responseTelefonos = null;
		RespBase<RespApiPersona.Telefono> responseTelefonoPersona = null;
		Map<String, Object> parametro = new HashedMap<>();
		String mensaje = "";

		responseTelefonos = personaApiClient.obtenerTelefonoBypersonaId(bean.getPersonaId());
		if (!responseTelefonos.getPayload().getItems().isEmpty()) {
			Long telefonoId = responseTelefonos.getPayload().getItems().get(0).getTelefonoId();
			String strTelefonoId = String.valueOf(telefonoId);
			Integer intTelefonoId = Integer.valueOf(strTelefonoId);
			RespBase<RespApiPersona.Telefono> telefono = personaApiClient.obtenerTelefono(intTelefonoId);

			telefono.getPayload().setNumeroTelefono(bean.getTelefono());
			requestTelefono = beanAdapter.adapToBeanTelefono(telefono);
			responseTelefonoPersona = personaApiClient.actualizaTelefono(strTelefonoId, requestTelefono);
			LOGGER.info("[responseTelefonoPersona: ] " + JsonUtil.convertirObjetoACadenaJson(responseTelefonoPersona));

			Optional<Persona> persona = personaRepository.findById(bean.getLongPersonaId());
			if (persona.isPresent()) {
				Persona personaUpdate = new Persona();
				persona.get().setTelefonoId(responseTelefonoPersona.getPayload().getTelefonoId());
				persona.get().setUsuarioModificacion(bean.getUsuario());
				persona.get().setFechaModificacion(Instant.now());
				// Actualizar telefono de la persona
				LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(personaUpdate));
				personaUpdate = personaRepository.save(persona.get());
			} else {
				mensaje = Constantes.MSG_NO_SE_ENCONTRO_LA_PERSONA_EN_EL_SERVIDOR_CIVIL;
			}

		} else {
			// Crear telefono
			RespBase<RespApiPersona.Telefono> telefono = new RespBase<>();
			RespApiPersona.Telefono telefonoDto = new RespApiPersona.Telefono();
			telefonoDto.setPersonaId(bean.getLongPersonaId());
			telefonoDto.setTipoTelefono(Constantes.TIPO_TELEFONO_CELULAR);

			telefonoDto.setCodigoArea(Constantes.VACIO);
			telefonoDto.setNumeroTelefono(bean.getTelefono());
			telefonoDto.setNumeroAnexo(Constantes.VACIO);

			telefono.setPayload(telefonoDto);
			requestTelefono = beanAdapter.adapToBeanTelefono(telefono);
			RespBase<RespApiPersona.Telefono> telefonoInsert = personaApiClient.crearTelefono(bean.getPersonaId(),
					requestTelefono);
			LOGGER.info("[telefonoInsert: ] " + JsonUtil.convertirObjetoACadenaJson(telefonoInsert));

			Optional<Persona> persona = personaRepository.findById(bean.getLongPersonaId());
			if (persona.isPresent()) {
				Persona personaUpdate = new Persona();
				persona.get().setTelefonoId(telefonoInsert.getPayload().getTelefonoId());
				persona.get().setUsuarioModificacion(bean.getUsuario());
				persona.get().setFechaModificacion(Instant.now());
				// Actualizar telefono de la persona
				LOGGER.info(Constantes.LOG_RESPONSESERVIDORES + JsonUtil.convertirObjetoACadenaJson(personaUpdate));
				personaUpdate = personaRepository.save(persona.get());
			} else {
				mensaje = Constantes.MSG_NO_SE_ENCONTRO_LA_PERSONA_EN_EL_SERVIDOR_CIVIL;
			}
		}

		parametro.put(Constantes.ESTADO, (mensaje.isEmpty()) ? Boolean.TRUE : Boolean.FALSE);
		parametro.put(Constantes.MENSAJE, mensaje);
		return parametro;
	}

	private Map<String, Object> actualizarFoto(LogoDTO foto, Empleado empleado) {
		Map<String, Object> parametroData = new HashedMap<>();
		String mensaje = "";

		// Cargar logo y guardar su imagen redimensionado
		RespBase<RespApiFile> responseWS = new RespBase<RespApiFile>();
		String rutaFoto = null;
		if (foto.getFileBase64() != null && foto.getFlag() == 0) {
			Parametro parametro = generalRepositorySP.buscarParametro(null, null, Constantes.RUTA_FILE_SERVER_ENTIDAD);
			String rutaFileServer = ParametrosUtil.datePathReplaceRepositoryAlfresco(parametro.getValorTexto())
					.substring(1);
			rutaFileServer = rutaFileServer.replace(Constantes.PATH_ENTIDAD, empleado.getEntidadId().toString());
			ReqBase<ApiFileServerDTO> requestApiUploadFile = new ReqBase<ApiFileServerDTO>();
			ApiFileServerDTO uploadFile = new ApiFileServerDTO();
			uploadFile.setExtension("." + ParametrosUtil.extension(foto.getFileName()));
			uploadFile.setFileBase64(foto.getFileBase64());
			uploadFile.setFileName(ParametrosUtil.onlyName(foto.getFileName()));
			uploadFile.setPath(rutaFileServer);

			BigDecimal decRatio = BigDecimal.ZERO;
			decRatio = new BigDecimal(Constantes.PERSONA_RATIO_DE_CAMBIO_FOTO);

			uploadFile.setRatioDeCambio(decRatio.doubleValue());
			uploadFile.setResize(true);
			requestApiUploadFile.setPayload(uploadFile);
			responseWS = maestraApiClient.insertImagen(requestApiUploadFile);
			rutaFoto = responseWS.getPayload().getPathRelative();
			parametroData.put("rutaFoto", rutaFoto);
			empleado.setUrlFoto(rutaFoto);
			if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
				mensaje = responseWS.getStatus().getError().toString();
			} else {
				parametroData.put("flag", 1);
			}

		} else {
			return new HashedMap<>();
		}

		parametroData.put(Constantes.ESTADO, (mensaje.isEmpty()) ? Boolean.TRUE : Boolean.FALSE);
		parametroData.put(Constantes.MENSAJE, mensaje);

		return parametroData;
	}

	private Map<String, Object> actualizarDetalleUnidadOrganica(PuestoActualizarDTO bean) {

		Map<String, Object> parametro = new HashedMap<>();
		String mensaje = "";

		try {

			Optional<DetUnidadOrganica> detUO = detUnidadOrganicaRepository.findById(bean.getDetuoId());
			if (detUO.isPresent()) {
				DetUnidadOrganica servCivil = new DetUnidadOrganica();
				detUO.get().setOrganigramaId(bean.getUoId());
				detUO.get().setPuestoId(bean.getPuestoId());
				detUO.get().setTipoAsignacion(bean.getTipoAsignacion());
				detUO.get().setFechaInicioPuesto(bean.getFechaInicio());
				detUO.get().setFechaCesePuesto(bean.getFechaCese());
				detUO.get().setUsuarioModificacion(bean.getUsuario());
				detUO.get().setFechaModificacion(Instant.now());

				if (bean.getFechaCese() != null) {
					detUO.get().setEstadoRegistro(Constantes.SERVIDOR_CIVIL_ESTADO_CESADO);
				}
				
				Empleado empleadoOpt = empleadoRepository.findByEntidadIdAndPersonaId(detUO.get().getEntidadId(), detUO.get().getPersonaId());
				if(empleadoOpt != null) {
					empleadoOpt.setPuestoId(detUO.get().getPuestoId());
					LOGGER.info("[puesto: ] " + JsonUtil.convertirObjetoACadenaJson(empleadoOpt));
					empleadoRepository.save(empleadoOpt);
				} else {
					mensaje = "Error en actualización del empleado, NO SE ENCUENTRA SERVIDOR CIVIL!";
					parametro.put(Constantes.OBJDETALLEUO, null);
					parametro.put(Constantes.ESTADO, Boolean.FALSE);
				}

				// Actualizar servidor civil
				servCivil = detUnidadOrganicaRepository.save(detUO.get());
				LOGGER.info("[puesto: ] " + JsonUtil.convertirObjetoACadenaJson(servCivil));

				mensaje = "Se realizó la actualización del servidor civil detalle del puesto exitosamente";
				parametro.put(Constantes.OBJDETALLEUO, servCivil);
				parametro.put(Constantes.ESTADO, Boolean.TRUE);
			} else {
				mensaje = "NO SE ENCUENTRA SERVIDOR CIVIL!";
				parametro.put(Constantes.OBJDETALLEUO, null);
				parametro.put(Constantes.ESTADO, Boolean.FALSE);
			}
		} catch (Exception e) {
			mensaje = "Error en la actualización del servidor civil detalle del puesto por inconsistencia";
			parametro.put(Constantes.OBJDETALLEUO, null);
			parametro.put(Constantes.ESTADO, Boolean.FALSE);
		}

		parametro.put(Constantes.MENSAJE, mensaje);
		return parametro;
	}

	@Override
	public RespBase<Object> editarParticipanteGdr(@Valid ReqBase<EditParticipanteGDRDTO> request, MyJsonWebToken jwt)
			throws Exception {
		RespBase<Object> response = new RespBase<>();

		Long detalleuoId = request.getPayload().getDetUoId();
		Optional<DetUnidadOrganica> responseById = editarParticipanteRepository.findById(detalleuoId);
		if (responseById.isPresent()) {
			DetUnidadOrganica bean = beanAdapter.adapToBeanUpdateDetUnidadOrganica(request.getPayload(), jwt,
					responseById.get());
			DetUnidadOrganica registrado = editarParticipanteRepository.save(bean);
			response.setPayload(registrado);
			response.getStatus().setSuccess(true);
			return response;
		} else {
			return ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.NO_ENCONTRADO);
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadoEvaluador> buscarEvaluadoEvaluador(Map<String, Object> parametroMap)
			throws ValidationException {
		try {
			EvaluadorEvaluadoDTO resultado = servidorCivilRepository.buscarEvaluadorEvaluado(parametroMap);
			RespBuscarParticipanteEvaluadoEvaluador respPayload = new RespBuscarParticipanteEvaluadoEvaluador();
			respPayload.setEvaluadorEvaluado(resultado);
			return new RespBase<RespBuscarParticipanteEvaluadoEvaluador>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipante> buscarParticipante(Map<String, Object> parametroMap)
			throws ValidationException {
		try {
			ParticipanteDTO resultado = servidorCivilRepository.buscarParticipante(parametroMap);
			RespBuscarParticipante respPayload = new RespBuscarParticipante();
			respPayload.setParticipante(resultado);
			return new RespBase<RespBuscarParticipante>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosPersonaServidorCivil(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadosPersonaServidorCivil(parametroMap);
			RespBuscarParticipanteEvaluadosServidorCivil respPayload = new RespBuscarParticipanteEvaluadosServidorCivil();
			respPayload.setListaParticipanteEvaluadosServidorCivil(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> obtenerDatosUsuarioEmail(Map<String, Object> parametroMap, MyJsonWebToken jwt) {

		RespApiCorreoPersona apiCorreoPersona = new RespApiCorreoPersona();
		RespBase<Object> response = new RespBase<>();

		DetUnidadOrganica detUOEvaluador = detUnidadOrganicaRepository.findByDetUnidadOrganicaId(
				(Long) parametroMap.get("detUoId"));

		if (detUOEvaluador != null) {
			List<CorreoPersonaDTO> correoEvaluador = servidorCivilRepository
					.buscarCorreoPersona((Long) parametroMap.get("personaEvaluadorId"));

			List<CorreoPersonaDTO> correoEvaluado = servidorCivilRepository
					.buscarCorreoPersona(detUOEvaluador.getPersonaId());
			if (!CollectionUtils.isEmpty(correoEvaluado)) {
				apiCorreoPersona.setCorreoEvaluado(correoEvaluado.get(0));
			}

			if (!CollectionUtils.isEmpty(correoEvaluador)) {
				apiCorreoPersona.setCorreoEvaluador(correoEvaluador.get(0));

				List<CorreoPersonaDTO> correoGestorORH = servidorCivilRepository
						.buscarCorreoPersonaGestor((Long) parametroMap.get("entidadId"), Constantes.TIPO_GESTOR_ORH);

				if (!CollectionUtils.isEmpty(correoGestorORH)) {
					apiCorreoPersona.setCorreoGestorORH(correoGestorORH.get(0));

					response.getStatus().setSuccess(Boolean.TRUE);
					response.setPayload(apiCorreoPersona);
					return response;
				} else {
//					return ParametrosUtil.setearResponse(response, Boolean.FALSE,
//							Constantes.NO_ENCONTRADO + " del Gestor ORH");
					LOGGER.info(Constantes.NO_ENCONTRADO + " del Gestor ORH");
					apiCorreoPersona.setCorreoGestorORH(null);

					response.getStatus().setSuccess(Boolean.TRUE);
					response.setPayload(apiCorreoPersona);
					return response;
				}
			} else {
				return ParametrosUtil.setearResponse(response, Boolean.FALSE,
						Constantes.NO_ENCONTRADO + " del Evaluador");
			}
		} else {
			return ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.NO_ENCONTRADO + " del Evaluador");
		}
	}

	@Override
	public RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil> buscarEvaluadosSinEvaluadoresEntidad(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<ParticipanteEvaluadosServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadoresServidorCivilEntidad(parametroMap);
			RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil respPayload = new RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil();
			respPayload.setCantidadSinEvaluar(lista != null ? lista.size() : 0);
			respPayload.setListaParticipanteEvaluadosSinEvaluador(lista);
			return new RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> buscarParticipantesEntidad(
			Map<String, Object> parametroMap) throws ValidationException {
			try {
				List<RespBuscarParticipantesEntidadServidorCivilDTO> listaNoRepeatDUOID;
				List<RespBuscarParticipantesEntidadServidorCivilDTO> lista = servidorCivilRepository
						.buscarParticipantesServidorCivilEntidad(parametroMap);
				listaNoRepeatDUOID = new ArrayList<>();
				for (int i = 0; i < lista.size(); i++) {
					RespBuscarParticipantesEntidadServidorCivilDTO cla = lista.get(i);
					while (i < lista.size()) {
						if (i + 1 < lista.size()) {
							RespBuscarParticipantesEntidadServidorCivilDTO next = lista.get(i + 1);
							if (cla.getDetUnidadOrganicaId().equals(next.getDetUnidadOrganicaId())) {
								i++;
							} else {
								listaNoRepeatDUOID.add(cla);
								break;
							}
						} else {
							listaNoRepeatDUOID.add(cla);
							break;
						}
					}
				}
				lista = listaNoRepeatDUOID;
				return new RespBase<Object>().ok(lista);
			} catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
				throw e;
			}
		
	}
	
	@Override
	public RespBase<Object> buscarParticipantesEvaluadosServidorCivilEntidad(
			Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO> lista = servidorCivilRepository
					.buscarParticipantesEvaluadosServidorCivilEntidad(parametroMap);
			return new RespBase<Object>().ok(lista);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}
	
	@Override
	public RespBase<Object> buscarParticipanteByDUOId(Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<RespParticipanteDTO> lista = servidorCivilRepository.buscarParticipanteByDUOId(parametroMap);
			return new RespBase<Object>().ok(lista);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}

	}
	
}