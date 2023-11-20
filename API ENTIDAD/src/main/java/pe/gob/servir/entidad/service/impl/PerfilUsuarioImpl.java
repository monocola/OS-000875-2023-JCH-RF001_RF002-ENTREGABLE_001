package pe.gob.servir.entidad.service.impl;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.lang3.StringUtils;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import pe.gob.servir.entidad.adapter.BeanAdapterPerfilUsuario;
import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiFileServerDTO;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.model.Correo;
import pe.gob.servir.entidad.model.Empleado;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.PerfilUsuarioDTO;
import pe.gob.servir.entidad.model.Persona;
import pe.gob.servir.entidad.repository.CorreoRepository;
import pe.gob.servir.entidad.repository.DetUnidadOrganicaRepository;
import pe.gob.servir.entidad.repository.EmpleadoRepository;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.PerfilUsuarioRepository;
import pe.gob.servir.entidad.repository.PersonaRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEditaPerfilUsuario;
import pe.gob.servir.entidad.request.dto.LogoDTO;
import pe.gob.servir.entidad.request.dto.PerfilUsuarioEdicionDTO;
import pe.gob.servir.entidad.response.RespApiFile;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerCorreo;
import pe.gob.servir.entidad.response.RespObtenerTelefono;
import pe.gob.servir.entidad.response.RespObtenerTelefono.Telefono;
import pe.gob.servir.entidad.response.RespObtienePerfilUsuario;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.PerfilUsuarioService;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class PerfilUsuarioImpl implements PerfilUsuarioService {

	private static final Logger LOGGER = Logger.getLogger(PerfilUsuarioImpl.class);

	@Autowired
	private BeanAdapterPerfilUsuario adapterPerfilUsuario;

	@Autowired
	PerfilUsuarioRepository perfilUsuarioRepository;

	@Autowired
	DetUnidadOrganicaRepository detUnidadOrganicaRepository;

	@Autowired
	EmpleadoRepository empleadoRepository;

	@Autowired
	PersonaApiClient personaApiClient;

	@Autowired
	PersonaRepository personaRepository;

	@Autowired
	CorreoRepository correoRepository;

	@Autowired
	BeanAdapterServidorCivil beanAdapter;

	@Autowired
	private MaestraApiClient maestraApiClient;

	@Autowired
	private GeneralRepository generalRepositorySP;

	@Override
	public RespBase<RespObtienePerfilUsuario> obtenerPerfilUsuario(Map<String, Object> parametroMap)
			throws ValidationException {
		try {
			List<PerfilUsuarioDTO> lista = perfilUsuarioRepository.obtenerPerfilUsuario(parametroMap);

			RespObtienePerfilUsuario out = new RespObtienePerfilUsuario();
			if (!CollectionUtils.isEmpty(lista)) {
				for (PerfilUsuarioDTO inServidor : lista) {
					adapterPerfilUsuario.adapToBeanResponsePerfilUsuario(out, inServidor);
				}
			}

			return new RespBase<RespObtienePerfilUsuario>().ok(out);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> editarPerfilUsuario(@Valid ReqBase<ReqEditaPerfilUsuario> request, MyJsonWebToken jwt)
			throws ValidationException {
		try {
			PerfilUsuarioEdicionDTO bean = new PerfilUsuarioEdicionDTO();
			RespBase<Object> response = new RespBase<>();
			String mensajeCorreo = "";
			String mensajeEmpleado = "";
			Map<String, Object> parametroData = null;

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

			mensajeEmpleado = actualizarDatosPerfilUsuario(bean, response, mensajeEmpleado);

			response = validarMensajeCorreo(response, mensajeCorreo, mensajeEmpleado);
			LOGGER.info(Constantes.LOG_RESPONSE + JsonUtil.convertirObjetoACadenaJson(response));

			return response;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	/********************************************************************************************************************
	 * 
	 * METODOS PRIVADOS
	 * 
	 * @param jwt
	 *
	 ********************************************************************************************************************/
	// Actualizar correo
	private Map<String, Object> actualizarCorreo(PerfilUsuarioEdicionDTO bean, MyJsonWebToken jwt) {

		RespBase<RespObtenerCorreo> responseCorreos = null;
		Map<String, Object> parametro = new HashedMap<>();
		String mensaje = "";

		responseCorreos = personaApiClient.obtenerCorreoBypersonaId(bean.getPersonaId());
		mensaje = validarCorreo(bean, responseCorreos, mensaje, jwt);

		parametro.put(Constantes.ESTADO, (mensaje != null && !mensaje.trim().isEmpty()) ? Boolean.FALSE : Boolean.TRUE);
		parametro.put(Constantes.MENSAJE, mensaje);

		return parametro;
	}

	private String validarCorreo(PerfilUsuarioEdicionDTO bean, RespBase<RespObtenerCorreo> responseCorreos,
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
			mensaje = " NO SE ENCONTRO CORREO DE LA PERSONA";
		}

		return mensaje;
	}

	private String actualizarDatosPerfilUsuario(PerfilUsuarioEdicionDTO bean, RespBase<Object> response,
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

		Empleado empleado = empleadoRepository.findByEntidadIdAndPersonaIdAndEstadoRegistro(bean.getEntidadId(), bean.getLongPersonaId(), Constantes.ESTADO_ACTIVO);
		
		if (empleado != null) {
			// Actualizar Empleado
			empleado.setRegimenLaboral(
					bean.getRegimenLaboralId() != null ? bean.getRegimenLaboralId() : empleado.getRegimenLaboral());
			empleado.setSindicatoFlag(bean.getSindicatoFlag());
			empleado.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(), bean.getUsuario(), Instant.now());

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

			empleadoRepository.save(empleado);
		} else {
			// Crear Empleado
			empleado = new Empleado();
			empleado.setRegimenLaboral(
					bean.getRegimenLaboralId() != null ? bean.getRegimenLaboralId() : empleado.getRegimenLaboral());
			empleado.setSindicatoFlag(bean.getSindicatoFlag());
			empleado.setEntidadId(bean.getEntidadId());
			empleado.setPersonaId(bean.getLongPersonaId());

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

			empleado.setCampoSegIns(bean.getUsuario(), Instant.now());
			empleadoRepository.save(empleado);
		}
		
		response.setPayload(empleado);

		return mensajeEmpleado;
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

	// Actualizar telefono
	private Map<String, Object> registrarTelefono(PerfilUsuarioEdicionDTO bean) {
		RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = null;
		RespBase<RespObtenerTelefono> responseTelefonos = null;
		RespBase<RespApiPersona.Telefono> responseTelefonoPersona = null;
		Map<String, Object> parametro = new HashedMap<>();
		String mensaje = "";

		responseTelefonos = personaApiClient.obtenerTelefonoBypersonaId(bean.getPersonaId());
		List<Telefono> filterTelefonos = responseTelefonos.getPayload().getItems().stream().filter(a -> Constantes.TIPO_TELEFONO_CELULAR.equals(a.getTipoTelefono())).collect(Collectors.toList());
		responseTelefonos.getPayload().setItems(filterTelefonos);
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

}