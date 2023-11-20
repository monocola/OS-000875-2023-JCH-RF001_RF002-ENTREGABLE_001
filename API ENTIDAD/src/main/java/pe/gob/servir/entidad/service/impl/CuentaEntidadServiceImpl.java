package pe.gob.servir.entidad.service.impl;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.googlecode.jmapper.JMapper;
import java.util.Optional;

import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiActualizarRolUsuario;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.ApiSeguridadRequestDTO;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.CorreoApiDTO;
import pe.gob.servir.entidad.model.CuentaEntidad;
import pe.gob.servir.entidad.model.ListaEntidades;
import pe.gob.servir.entidad.model.ObtenerCuentaEntidadRolDTO;
import pe.gob.servir.entidad.model.ObtenerRolDTO;
import pe.gob.servir.entidad.model.ValidaTareaCuentaEntidad;
import pe.gob.servir.entidad.model.ValidarUsuarioDTO;
import pe.gob.servir.entidad.repository.CorreoRepositorySP;
import pe.gob.servir.entidad.repository.CuentaEntidadRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.request.ReqActualizaCuentaEntidad;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqCreaCuentaEntidad;
import pe.gob.servir.entidad.request.dto.CuentaEntidadDTO;
import pe.gob.servir.entidad.request.dto.TelefonoDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespApiSeguridad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespComboUsuarioPorEntidadRol;
import pe.gob.servir.entidad.response.RespInactivarCuenta;
import pe.gob.servir.entidad.response.RespListaEntidades;
import pe.gob.servir.entidad.response.RespValidaTarea;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.CuentaEntidadService;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.util.ParametrosUtil;
import pe.gob.servir.entidad.request.dto.PersonaNaturalDTO;
import pe.gob.servir.entidad.response.RespReasignarCuentaEntidad;
import pe.gob.servir.entidad.request.dto.CorreoDTO;
import pe.gob.servir.entidad.service.NotificacionService;
import pe.gob.servir.entidad.model.ValidaPersonaCuenta;

@Service
public class CuentaEntidadServiceImpl implements CuentaEntidadService {
	
	@Autowired
	private GestionRepository gestionRepository;
	
	@Autowired
	private CuentaEntidadRepository cuentaEntidadRepository;
	
	@Autowired
	private PersonaService personaService; 
	
	@Autowired
	private PersonaApiClient personaApiClient;
	
	@Autowired
	private SeguridadApiClient seguridadApiClient; 

	@Autowired
	private NotificacionService notificacionService;
	
	@Autowired
	private VariablesSistema variablesSistema;
		
	@Autowired
	private CorreoRepositorySP correoRepositorySP;

	@Autowired
	private BeanAdapterServidorCivil beanAdapterServidorCivil; 
	
	@Override
	public RespBase<RespListaEntidades> buscarEntidadesAsociadas(Map<String, Object> parametroMap) {
		RespListaEntidades respPayload = new RespListaEntidades();
		List<RespListaEntidades.Entidades> listaEntidadesByRol =  new ArrayList<>();	
		List<ListaEntidades> listaEntidades = gestionRepository.listaEntidadesByRol(parametroMap);	
		List<ObtenerRolDTO> listaRoles = gestionRepository.listaRolesByEntidades(parametroMap);
		generarListaCuentaAsociadad(listaEntidades, listaRoles, listaEntidadesByRol);
		respPayload.setListaEntidades(listaEntidadesByRol);
		return new RespBase<RespListaEntidades>().ok(respPayload);
	}

	@SuppressWarnings("rawtypes")
	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> registrarCuentEnti(ReqBase<ReqCreaCuentaEntidad> request, MyJsonWebToken token) {//NOSONAR
		RespBase<Object> response = new RespBase<>();
		Long telefonoId = null;
		Long usuarioId = null;	
		Long correoId = null;
		JMapper<CuentaEntidad, CuentaEntidadDTO> cuentaEntidadMapper = new JMapper<>(CuentaEntidad.class,
				CuentaEntidadDTO.class);
		CuentaEntidad cuentaEntidad = cuentaEntidadMapper.getDestination(request.getPayload().getCuentaEntidad());
		try {			
			if(cuentaEntidad.getPersonaId()!= null) {
				List<ValidaPersonaCuenta> validaPersonaExistente = gestionRepository.validarPersonaCuentaEntidad(cuentaEntidad.getPersonaId());
				if(validaPersonaExistente.isEmpty()) {					
					RespBase<RespApiPersona> buscarPersona = personaApiClient.obtenerPersonaById(cuentaEntidad.getPersonaId().intValue());
					if(Boolean.FALSE.equals(buscarPersona.getStatus().getSuccess())) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, buscarPersona.getStatus().getError().toString());
						return response;
					}
					RespApiPersona personaResponse = buscarPersona.getPayload();
					List<RespApiPersona.Correo> correosResponse = new ArrayList<>();
					RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<>();
					PersonaNaturalDTO persona = request.getPayload().getPersonaNatural();
					ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(null,Constantes.TIPO_CORREO_PRINCIPAL, persona.getCorreoPrincipal());
					requestCorreo.setPayload(correoPayload);
					RespBase<RespApiPersona.Correo> respuestaCorreo = personaApiClient.crearCorreo(cuentaEntidad.getPersonaId().intValue(), requestCorreo);
					if(Boolean.FALSE.equals(respuestaCorreo.getStatus().getSuccess())) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaCorreo.getStatus().getError().toString());
						return response;
					}else {
						correoId = respuestaCorreo.getPayload().getCorreoId();
						correosResponse.add(respuestaCorreo.getPayload());
					}
					List<RespApiPersona.Telefono> telefonoResponse = new ArrayList<>();
					RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = new RespBase<>();					
					ApiPersonaRequestDTO.Telefono telefonoPayload = new ApiPersonaRequestDTO.Telefono(null, Constantes.TIPO_TELEFONO_CASA, null, persona.getTelefonoFijo(), persona.getAnexo());
					requestTelefono.setPayload(telefonoPayload);
					RespBase<RespApiPersona.Telefono> respuestaTelefono = personaApiClient.crearTelefono(cuentaEntidad.getPersonaId().intValue(), requestTelefono);
					if(Boolean.FALSE.equals(respuestaTelefono.getStatus().getSuccess())) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaTelefono.getStatus().getError().toString());
						return response;
					}else {
						telefonoId = respuestaTelefono.getPayload().getTelefonoId();
						telefonoResponse.add(respuestaTelefono.getPayload());
					}
					personaResponse.setCorreos(correosResponse);
					personaResponse.setTelefonos(telefonoResponse);
					List<AsignaRolRequestDTO> listaRoles = request.getPayload().getListaRoles();
					int i = 0;					
					List<ValidarUsuarioDTO> obtenerUsuarioResponse = gestionRepository.ObtenerUsuarioExistente(personaResponse.getDocumentos().get(0).getNumeroDocumento());
					for (AsignaRolRequestDTO asignaRolRequestDTO : listaRoles) {						
						if(asignaRolRequestDTO.getRolId() != null){													
								if(obtenerUsuarioResponse.isEmpty()) {
									if(i == 0) {
										RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
										ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(personaResponse.getDocumentos().get(0).getNumeroDocumento(),personaResponse.getCorreos().get(0).getCorreo(), personaResponse.getPersona().getPersonaId(), Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, null);
										apiUsuario.setPayload(requestCreaUsuario);

										RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(cuentaEntidad.getEntidadId(),apiUsuario);

										if(Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
											response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaCreaUser.getStatus().getError().toString());
											return response;
										}else {
											usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
										}
									}
								}else {	
									for (ValidarUsuarioDTO obtenerUsuario : obtenerUsuarioResponse) {
										if(i == 0) {
											response = seguridadApiClient.asignarUsuarioEntidad(cuentaEntidad.getEntidadId(), obtenerUsuario.getUsuarioId().longValue(), Constantes.ESTADO_ACTIVO);
											if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
												return response;
											}else {
													usuarioId = obtenerUsuario.getUsuarioId().longValue();
											}
											enviarCorreoExisteUsuario(personaResponse.getCorreos().get(0).getCorreo(),obtenerUsuario.getCorreoElectronico(),obtenerUsuario.getUsuario(),false);
										}
									}	
								}
							RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
							AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
							asignaRolDTO.setUsuarioId(usuarioId);
							asignaRolDTO.setRolId(asignaRolRequestDTO.getRolId());
							requestAsignaRol.setPayload(asignaRolDTO);
							response = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
							if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
								return response;
							}
						}else {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.NO_ROL);
							return response;
						}
						i++;
					}				
				}else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.MSJ_ENTIDAD + validaPersonaExistente.get(0).getNombreEntidad());
					return response;
				}
			}else {
				Long personaId = null;					
				RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
				PersonaNaturalDTO personaNatural =  request.getPayload().getPersonaNatural();
				if(personaNatural.getTipoDocumento().equals(variablesSistema.tipoDocumentoDni) ) {
					personaNatural.setPaisId(variablesSistema.idPaisPeru);
				}
				ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();				
				ParametrosUtil.setearPersonaNatural(apiPersonaNatural,getPersonaDTO(personaNatural));
				List<ApiPersonaRequestDTO.Correo> correosPersona = new ArrayList<>(apiPersonaNatural.getCorreos());
				personaNaturalResquest.setPayload(apiPersonaNatural);	
				response = personaService.obtenerInsertarPersona(variablesSistema.tipoPersonaNatural,personaNatural.getTipoDocumento(), personaNatural.getNumeroDocumento(), personaNaturalResquest);
				if(Boolean.FALSE.equals(response.getStatus().getSuccess())){
					return response;
				}
				RespApiPersona personaResponse = (RespApiPersona) response.getPayload();
				personaId = personaResponse.getPersona().getPersonaId();
				telefonoId = (personaResponse.getTelefonos().isEmpty()?telefonoId:personaResponse.getTelefonos().get(0).getTelefonoId());
				List<RespApiPersona.Correo> correosResponse = new ArrayList<>();
				for (ApiPersonaRequestDTO.Correo correoPersona : correosPersona) {
					List<CorreoApiDTO> correoRpta = correoRepositorySP.buscarEmailPersona(correoPersona.getCorreo());	
					if(correoRpta.isEmpty()){
						RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<>();
						ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(null,correoPersona.getTipoCorreo(), correoPersona.getCorreo());
						requestCorreo.setPayload(correoPayload);
						RespBase<RespApiPersona.Correo> respuesta = personaApiClient.crearCorreo(personaResponse.getPersona().getPersonaId().intValue(),requestCorreo);
						if(Boolean.TRUE.equals(respuesta.getStatus().getSuccess())) {
							correoId = respuesta.getPayload().getCorreoId();
							correosResponse.add(respuesta.getPayload());
						}else {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuesta.getStatus().getError().toString());
							return response;
						}
					}else {
						correosResponse.add(beanAdapterServidorCivil.adapterCorreo(correoRpta));
						correoId=correoRpta.get(0).getCorreoId();
					}
				}					
				personaResponse.setCorreos(correosResponse);				
				List<AsignaRolRequestDTO> listaRoles = request.getPayload().getListaRoles();
				List<ValidarUsuarioDTO> obtenerUsuarioResponse = gestionRepository.ObtenerUsuarioExistente(personaResponse.getDocumentos().get(0).getNumeroDocumento());
				int i = 0;
				for (AsignaRolRequestDTO asignaRolRequestDTO : listaRoles) {
					if(asignaRolRequestDTO.getRolId() != null){												
							if(obtenerUsuarioResponse.isEmpty()) {
								if(i==0) {
									RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
									ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(personaResponse.getDocumentos().get(0).getNumeroDocumento(),personaResponse.getCorreos().get(0).getCorreo(), personaResponse.getPersona().getPersonaId(), Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, null);
									apiUsuario.setPayload(requestCreaUsuario);
									RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(cuentaEntidad.getEntidadId(),apiUsuario);
									if(Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
										response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaCreaUser.getStatus().getError().toString());
										return response;
									}else {
										usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
									}
								}
							}else {									
								for (ValidarUsuarioDTO obtenerUsuario : obtenerUsuarioResponse) {
									if(i==0) {
										response = seguridadApiClient.asignarUsuarioEntidad(cuentaEntidad.getEntidadId(), obtenerUsuario.getUsuarioId().longValue(), Constantes.ESTADO_ACTIVO);
										if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
											return response;
										}else {
												usuarioId = obtenerUsuario.getUsuarioId().longValue();
												
										}
										enviarCorreoExisteUsuario(personaResponse.getCorreos().get(0).getCorreo(),obtenerUsuario.getCorreoElectronico(),obtenerUsuario.getUsuario(),false);		
									}
								}							
							}
						AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
						RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
						asignaRolDTO.setRolId(asignaRolRequestDTO.getRolId());
						asignaRolDTO.setUsuarioId(usuarioId);
						requestAsignaRol.setPayload(asignaRolDTO);
						response = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
						if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
							return response;
						}
					}else {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.NO_ROL);
						return response;
					}
					i++;
				}			
				cuentaEntidad.setPersonaId(personaId);					
			}
			cuentaEntidad.setTelefonoId(telefonoId);
			cuentaEntidad.setCorreoId(correoId); 
			cuentaEntidad.setUsuarioId(usuarioId);
			cuentaEntidad.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			cuentaEntidadRepository.save(cuentaEntidad);
			response.setPayload(cuentaEntidad);	
		}catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;
	}
	public PersonaDTO getPersonaDTO(PersonaNaturalDTO person) {

		return new PersonaDTO(person.getTipoDocumento(),person.getNumeroDocumento(),person.getNombres(),person.getApellidoPaterno(),person.getApellidoMaterno(),person.getApellidoCasada(),person.getSexo(),person.getEstadoCivil(),null,null,person.getPaisId(),null,person.getDireccionCompleta(),
				 null,person.getCorreoPrincipal(),null,person.getCorreoSecundario(),person.getCorreoLaboral(),
				 null,person.getTelefonoFijo(),person.getAnexo(),null,person.getCelularPrincipal(),person.getCelularSecundario(),person.getCelularLaboral(),
				 person.getRutaPaginaWeb());
	}

	
	private void generarListaCuentaAsociadad(List<ListaEntidades> listaCuenta, List<ObtenerRolDTO> listaRoles, List<RespListaEntidades.Entidades> responseCuentaRoles) {
		
		RespListaEntidades.Entidades entidad;
		for (ListaEntidades entidades : listaCuenta) {
			entidad = new RespListaEntidades.Entidades();			
			entidad.setNroCuentas(entidades.getNroCuentas());
			entidad.setCuentaId(entidades.getCuentaId());
			entidad.setUsuarioId(entidades.getUsuarioId());
			entidad.setPersonaId(entidades.getPersonaId());
			entidad.setNombres(entidades.getNombres());
			entidad.setApellidoPaterno(entidades.getApellidoPaterno());
			entidad.setApellidoMaterno(entidades.getApellidoMaterno());
			entidad.setDescripcionPuesto(entidades.getDescripcionPuesto());
			entidad.setDocumentoId(entidades.getDocumentoId());
			entidad.setTipoDocumento(entidades.getTipoDocumento());
			entidad.setDescTipoDocumento(entidades.getDescTipoDocumento());
			entidad.setNroDocumento(entidades.getNroDocumento());
			entidad.setEstadoId(entidades.getEstadoId());
			entidad.setDescripcionEstado(entidades.getDescripcionEstado());
			entidad.setPaisId(entidades.getPaisId());
			entidad.setNombrePais(entidades.getNombrePais());
			entidad.setCorreoId(entidades.getCorreoId());
			entidad.setCorreo(entidades.getCorreo());
			entidad.setTelefonoId(entidades.getTelefonoId());
			entidad.setNumeroTelefono(entidades.getNumeroTelefono());
			entidad.setAnexo(entidades.getAnexo());
			entidad.setFechaAlta(entidades.getFechaAlta());
			entidad.setFechaBaja(entidades.getFechaBaja());
			entidad.setFlagCuentaEditable(entidades.getFlagCuentaEditable());			
			List<RespListaEntidades.Roles> listRoles =  new ArrayList<>();
			for (ObtenerRolDTO obtenerRolDTO : listaRoles) {
				RespListaEntidades.Roles rol;
				if(obtenerRolDTO.getCuentaId().toString().equals(entidades.getCuentaId().toString())) {
					rol  = new RespListaEntidades.Roles();
					rol.setNroRoles(obtenerRolDTO.getNroRoles());
					rol.setUsuarioRolId(obtenerRolDTO.getUsuarioRolId());
					rol.setRolId(obtenerRolDTO.getRolId());
					rol.setNombreRol(obtenerRolDTO.getNombreRol());
					rol.setCuentaId(obtenerRolDTO.getCuentaId());
					rol.setEstadoId(obtenerRolDTO.getEstadoId());
					rol.setDescripcionEstado(obtenerRolDTO.getDescripcionEstado());
					rol.setFechaAltaRol(obtenerRolDTO.getFechaAltaRol());
					rol.setFechaBajaRol(obtenerRolDTO.getFechaBajaRol());
					listRoles.add(rol);
				}
				entidad.setListaRoles(listRoles);
			}
			responseCuentaRoles.add(entidad);
			
		}
		
		
	}

	
	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> actualizarCuentaEntidad(ReqBase<ReqActualizaCuentaEntidad> request, MyJsonWebToken token, Long cuentaEntidadId) {//NOSONAR
		RespBase<Object> response = new RespBase<>();
		Optional<CuentaEntidad> cuentaEntidad = cuentaEntidadRepository.findById(cuentaEntidadId); 
		try {
			if(cuentaEntidad.isPresent()) {
				CuentaEntidad ctaEntidad = cuentaEntidad.get();
				ctaEntidad.setPuestoTrabajoId(request.getPayload().getCuentaEntidad().getPuestoTrabajoId() != null ? 
						request.getPayload().getCuentaEntidad().getPuestoTrabajoId() : ctaEntidad.getPuestoTrabajoId());			
				ctaEntidad.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(),token.getUsuario().getUsuario(), Instant.now());
				
				if(ctaEntidad.getCorreoId() != null) {
					RespBase<RespApiPersona.Correo> respuesta = personaApiClient.obtenerCorreo(ctaEntidad.getCorreoId().intValue());
					if(Boolean.TRUE.equals(respuesta.getStatus().getSuccess())) {
						RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<>();
						CorreoDTO correoPersona = request.getPayload().getCorreo();
						ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(correoPersona.getCorreoId(),correoPersona.getTipoCorreo(), correoPersona.getCorreo());
						requestCorreo.setPayload(correoPayload);
						RespBase<RespApiPersona.Correo> respuestaCorreo = personaApiClient.actualizaCorreo(ctaEntidad.getCorreoId().toString(), requestCorreo);
						if(Boolean.FALSE.equals(respuestaCorreo.getStatus().getSuccess())) {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaCorreo.getStatus().getError().toString());
							return response;
						}
					}else {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuesta.getStatus().getError().toString());
						return response;
					}
				}
				
				if(ctaEntidad.getTelefonoId() != null) {
					RespBase<RespApiPersona.Telefono> respuesta = personaApiClient.obtenerTelefono(ctaEntidad.getTelefonoId().intValue());
					if(Boolean.TRUE.equals(respuesta.getStatus().getSuccess())) {
						RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = new RespBase<>();
						TelefonoDTO telefonoPersona = request.getPayload().getTelefono();
						ApiPersonaRequestDTO.Telefono telefonoPayload = new ApiPersonaRequestDTO.Telefono(telefonoPersona.getTelefonoId(), telefonoPersona.getTipoTelefono(), telefonoPersona.getCodigoArea(), telefonoPersona.getNumeroTelefono(), telefonoPersona.getNumeroAnexo());
						requestTelefono.setPayload(telefonoPayload);
						RespBase<RespApiPersona.Telefono> respuestaTelefono = personaApiClient.actualizaTelefono(ctaEntidad.getTelefonoId().toString(), requestTelefono);
						if(Boolean.FALSE.equals(respuestaTelefono.getStatus().getSuccess())) {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaTelefono.getStatus().getError().toString());
							return response;
						}
					}else {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuesta.getStatus().getError().toString());
						return response;
					}
				}
				Map<String, Object> parametroMap = new HashMap<>();	
				parametroMap.put("cuentaId", ctaEntidad.getEntidadId().intValue());	
				parametroMap.put("aplicacionId", variablesSistema.aplicacionTalentoId.intValue());
				List<ObtenerRolDTO> listaRoles = gestionRepository.listaRolesByEntidades(parametroMap);
				List<AsignaRolRequestDTO> roles = request.getPayload().getListaRoles();
				for (ObtenerRolDTO dto : listaRoles) {
					boolean encontro = false;					
					ApiActualizarRolUsuario rolUsuario;
					RespBase<ApiActualizarRolUsuario> requestRolUsuario;
					if(dto.getCuentaId().toString().equals(ctaEntidad.getCuentaEntidadId().toString())) {	
						for(AsignaRolRequestDTO rol : roles) {								
							if(dto.getRolId().toString().equals(rol.getRolId().toString()) ) {
								encontro = true;
								break;

							}else {
								encontro = false;
							}
						}
						if(!encontro) {
							rolUsuario = new ApiActualizarRolUsuario();
							requestRolUsuario =  new RespBase<>();
							rolUsuario.setUsuarioRolId(dto.getUsuarioRolId());
							rolUsuario.setFechaInicioVigencia(dto.getFechaAltaRol());
							rolUsuario.setFechaFinVigencia(ParametrosUtil.fechaHoraActualString());
							rolUsuario.setEstado(EstadoRegistro.INACTIVO.getCodigo());
							requestRolUsuario.setPayload(rolUsuario);
							response = seguridadApiClient.actualizarEstadoRol(rolUsuario.getUsuarioRolId(), requestRolUsuario);								
						}				
					}
				}
				Long usuarioId = null;
				List<AsignaRolRequestDTO> listaRolesDTO = request.getPayload().getListaRoles();
				RespBase<RespApiPersona> obtenerPersona = personaApiClient.obtenerPersonaById(ctaEntidad.getPersonaId().intValue()); 
				RespApiPersona personaResponse = (RespApiPersona) obtenerPersona.getPayload();//NOSONAR
				for (AsignaRolRequestDTO asignaRolRequestDTO : listaRolesDTO) {
					if(asignaRolRequestDTO.getRolId() != null){
						List<ValidarUsuarioDTO> obtenerUsuarioResponse = gestionRepository.ObtenerUsuarioExistente(personaResponse.getDocumentos().get(0).getNumeroDocumento());						
							if(obtenerUsuarioResponse.isEmpty()) {
								RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
								ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(personaResponse.getDocumentos().get(0).getNumeroDocumento(),personaResponse.getCorreos().get(0).getCorreo(), personaResponse.getPersona().getPersonaId(), Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, null);
								apiUsuario.setPayload(requestCreaUsuario);
								RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(ctaEntidad.getEntidadId(),apiUsuario);
								if(Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
									response = ParametrosUtil.setearResponse(response, Boolean.FALSE, respuestaCreaUser.getStatus().getError().toString());
									return response;
								}else {
									usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
								}
							}else {	
								for (ValidarUsuarioDTO obtenerUsuario : obtenerUsuarioResponse) {
									response = seguridadApiClient.asignarUsuarioEntidad(ctaEntidad.getEntidadId(), obtenerUsuario.getUsuarioId().longValue(), Constantes.ESTADO_ACTIVO);
									if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
										return response;
									}else {
											usuarioId = obtenerUsuario.getUsuarioId().longValue();
									}
								}	
							}
						RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
						AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
						asignaRolDTO.setRolId(asignaRolRequestDTO.getRolId());
						asignaRolDTO.setUsuarioId(usuarioId);
						requestAsignaRol.setPayload(asignaRolDTO);
						response = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
						if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
							return response;
						}
					}else {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.NO_ROL);
					}
				}
				ctaEntidad.setUsuarioId(usuarioId);
				cuentaEntidadRepository.save(ctaEntidad);
				response.setPayload(ctaEntidad);
			}
		}catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, e.getMessage());
		}
		return response;
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespInactivarCuenta> inactivarCuentaEntidad(Long cuentaId,String estado,MyJsonWebToken token) {
		RespBase<Object> response;
		Optional<CuentaEntidad> cuentaEntidad = cuentaEntidadRepository.findById(cuentaId); 
		RespBase<RespInactivarCuenta> responseCuenta = new RespBase<>();
		RespInactivarCuenta respPayload = new RespInactivarCuenta();		
		if(cuentaEntidad.isPresent()) {
			CuentaEntidad ctaEntidad = cuentaEntidad.get();							
			ctaEntidad.setCampoSegUpd(estado, token.getUsuario().getUsuario(), Instant.now());
			Map<String, Object> parametroMap = new HashMap<>();	
			parametroMap.put("cuentaId", ctaEntidad.getEntidadId().intValue());
			parametroMap.put("aplicacionId", variablesSistema.aplicacionTalentoId.intValue());
			List<ObtenerRolDTO> listaRoles = gestionRepository.listaRolesByEntidades(parametroMap);
			List<ApiActualizarRolUsuario> listaRolUsuario =  new ArrayList<>();
			for (ObtenerRolDTO dto : listaRoles) {
				ApiActualizarRolUsuario rolUsuario;
				RespBase<ApiActualizarRolUsuario> requestRolUsuario;
				if(dto.getCuentaId().toString().equals(ctaEntidad.getCuentaEntidadId().toString())) {
					rolUsuario = new ApiActualizarRolUsuario();
					requestRolUsuario =  new RespBase<>();
					rolUsuario.setUsuarioRolId(dto.getUsuarioRolId());
					rolUsuario.setFechaInicioVigencia(dto.getFechaAltaRol());
					rolUsuario.setFechaFinVigencia(ParametrosUtil.fechaHoraActualString());
					rolUsuario.setEstado(estado);
					requestRolUsuario.setPayload(rolUsuario);
					response = seguridadApiClient.actualizarEstadoRol(rolUsuario.getUsuarioRolId(), requestRolUsuario);						
					listaRolUsuario.add(rolUsuario);
					if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
						responseCuenta = ParametrosUtil.setearResponse(responseCuenta, Boolean.FALSE, response.getStatus().getError().toString());
						return responseCuenta;
					}
				}
			}
			cuentaEntidadRepository.save(ctaEntidad);
			respPayload.setCuentaEntidad(ctaEntidad);
			respPayload.setRolUsuarios(listaRolUsuario);
			
		}		
		return new RespBase<RespInactivarCuenta>().ok(respPayload);		
	}

	@Override
	public RespBase<RespValidaTarea> validaTareaCuentaEntidad(Long cuentaEntidadId) {
		List<ValidaTareaCuentaEntidad> validaCuenta = gestionRepository.validarTareaByCuneta(cuentaEntidadId);
		RespValidaTarea respPayload = new RespValidaTarea();
		respPayload.setTareaCuentaEntidad(validaCuenta);
		return new RespBase<RespValidaTarea>().ok(respPayload);
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@SuppressWarnings("rawtypes")
	@Override
	public RespBase<RespReasignarCuentaEntidad> reAsignarCuentaEntidad(Long cuentaId,ReqBase<ReqCreaCuentaEntidad> request,//NOSONAR
			MyJsonWebToken token) {		
		
		RespReasignarCuentaEntidad reasignarPayload  = new RespReasignarCuentaEntidad();
		RespBase<RespReasignarCuentaEntidad> responseCuentaEntidad = new RespBase<>();
		RespBase<RespInactivarCuenta> inactivarCuentaActual =  inactivarCuentaEntidad(cuentaId, EstadoRegistro.INACTIVO.getCodigo(), token);
		if(Boolean.FALSE.equals(inactivarCuentaActual.getStatus().getSuccess())) {
			responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, inactivarCuentaActual.getStatus().getError().toString());
			return responseCuentaEntidad;
		}
		RespBase<Object> response;
		JMapper<CuentaEntidad, CuentaEntidadDTO> cuentaEntidadMapper = new JMapper<>(CuentaEntidad.class,
				CuentaEntidadDTO.class);
		CuentaEntidad cuentaEntidad = cuentaEntidadMapper.getDestination(request.getPayload().getCuentaEntidad());
		Optional<CuentaEntidad> cuentaEntidadAsignar = cuentaEntidadRepository.findById(cuentaId); 
		Long usuarioId = null;	
		Long telefonoId = null;
		Long correoId = null;
		Long personaId = null;
		if(cuentaEntidadAsignar.isPresent()) {
			CuentaEntidad ctaEntidadAsignar = cuentaEntidadAsignar.get();
			if(cuentaEntidad.getPersonaId()!= null) {
				personaId = cuentaEntidad.getPersonaId(); 
				ctaEntidadAsignar.setPersonaId(personaId);
				List<ValidaPersonaCuenta> validaPersonaExistente = gestionRepository.validarPersonaCuentaEntidad(cuentaEntidad.getPersonaId());
				if(validaPersonaExistente.isEmpty()) {					
					RespBase<RespApiPersona> buscarPersona = personaApiClient.obtenerPersonaById(cuentaEntidad.getPersonaId().intValue());
					if(Boolean.FALSE.equals(buscarPersona.getStatus().getSuccess())) {
						responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, buscarPersona.getStatus().getError().toString());
						return responseCuentaEntidad;
					}
					RespApiPersona personaResponse = buscarPersona.getPayload();
					List<RespApiPersona.Correo> correosResponse = new ArrayList<>();
					RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<>();
					PersonaNaturalDTO persona = request.getPayload().getPersonaNatural();
					ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(null,Constantes.TIPO_CORREO_PRINCIPAL, persona.getCorreoPrincipal());
					requestCorreo.setPayload(correoPayload);
					RespBase<RespApiPersona.Correo> respuestaCorreo = personaApiClient.crearCorreo(cuentaEntidad.getPersonaId().intValue(), requestCorreo);
					if(Boolean.FALSE.equals(respuestaCorreo.getStatus().getSuccess())) {
						responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, respuestaCorreo.getStatus().getError().toString());
						return responseCuentaEntidad;
					}else {
						correoId = respuestaCorreo.getPayload().getCorreoId();
						correosResponse.add(respuestaCorreo.getPayload());
					}
					List<RespApiPersona.Telefono> telefonoResponse = new ArrayList<>();
					RespBase<ApiPersonaRequestDTO.Telefono> requestTelefono = new RespBase<>();					
					ApiPersonaRequestDTO.Telefono telefonoPayload = new ApiPersonaRequestDTO.Telefono(null, Constantes.TIPO_TELEFONO_CASA, null, persona.getTelefonoFijo(), persona.getAnexo());
					requestTelefono.setPayload(telefonoPayload);
					RespBase<RespApiPersona.Telefono> respuestaTelefono = personaApiClient.crearTelefono(cuentaEntidad.getPersonaId().intValue(), requestTelefono);
					if(Boolean.FALSE.equals(respuestaTelefono.getStatus().getSuccess())) {
						responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, respuestaTelefono.getStatus().getError().toString());
						return responseCuentaEntidad;
					}else {
						telefonoId = respuestaTelefono.getPayload().getTelefonoId();
						telefonoResponse.add(respuestaTelefono.getPayload());
					}
					personaResponse.setCorreos(correosResponse);
					personaResponse.setTelefonos(telefonoResponse);
	
					List<AsignaRolRequestDTO> listaRoles = request.getPayload().getListaRoles();
					int i = 0;
					List<ValidarUsuarioDTO> obtenerUsuarioResponse = gestionRepository.ObtenerUsuarioExistente(personaResponse.getDocumentos().get(0).getNumeroDocumento());
					for (AsignaRolRequestDTO asignaRolRequestDTO : listaRoles) {						
						if(asignaRolRequestDTO.getRolId() != null){													
								if(obtenerUsuarioResponse.isEmpty()) {
									if(i == 0) {
										RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
										ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(personaResponse.getDocumentos().get(0).getNumeroDocumento(),personaResponse.getCorreos().get(0).getCorreo(), personaResponse.getPersona().getPersonaId(), Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, null);
										apiUsuario.setPayload(requestCreaUsuario);
										RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(cuentaEntidad.getEntidadId(),apiUsuario);
										if(Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
											responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, respuestaCreaUser.getStatus().getError().toString());
											return responseCuentaEntidad;
										}else {
											usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
										}
									}
								}else {	
									for (ValidarUsuarioDTO obtenerUsuario : obtenerUsuarioResponse) {
										if(i == 0) {
											response = seguridadApiClient.asignarUsuarioEntidad(cuentaEntidad.getEntidadId(), obtenerUsuario.getUsuarioId().longValue(), Constantes.ESTADO_ACTIVO);
											if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
												responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, response.getStatus().getError().toString());
												return responseCuentaEntidad;											
											}else {
													usuarioId = obtenerUsuario.getUsuarioId().longValue();
											}
											enviarCorreoExisteUsuario(personaResponse.getCorreos().get(0).getCorreo(),obtenerUsuario.getCorreoElectronico(),obtenerUsuario.getUsuario(),false);
										}
									}	
								}
							RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
							AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
							asignaRolDTO.setRolId(asignaRolRequestDTO.getRolId());
							asignaRolDTO.setUsuarioId(usuarioId);
							requestAsignaRol.setPayload(asignaRolDTO);
							response = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
							if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
								responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, response.getStatus().getError().toString());
								return responseCuentaEntidad;
							}
						}else {
							responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, "NO SE ENCONTRO EL ROL");
							return responseCuentaEntidad;
						}
						i++;
					}				
				}else {
					responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, "LA PERSONA YA EXISTE EN UNA ENTIDAD " + validaPersonaExistente.get(0).getNombreEntidad());
					return responseCuentaEntidad;
				}
			}else {
							
				RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
				PersonaNaturalDTO personaNatural =  request.getPayload().getPersonaNatural();
				if(personaNatural.getTipoDocumento().equals(variablesSistema.tipoDocumentoDni)) {
					personaNatural.setPaisId(variablesSistema.idPaisPeru);
				}
				ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();				
				ParametrosUtil.setearPersonaNatural(apiPersonaNatural,getPersonaDTO(personaNatural));
				List<ApiPersonaRequestDTO.Correo> correosPersona = new ArrayList<>(apiPersonaNatural.getCorreos());
				apiPersonaNatural.getCorreos().clear();					
				personaNaturalResquest.setPayload(apiPersonaNatural);	
				response = personaService.obtenerInsertarPersona(variablesSistema.tipoPersonaNatural,personaNatural.getTipoDocumento(), personaNatural.getNumeroDocumento(), personaNaturalResquest);
				if(Boolean.FALSE.equals(response.getStatus().getSuccess())){
					responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, response.getStatus().getError().toString());
					return responseCuentaEntidad;
				}
				RespApiPersona personaResponse = (RespApiPersona) response.getPayload();
				personaId = personaResponse.getPersona().getPersonaId();			
				telefonoId = (personaResponse.getTelefonos().isEmpty()?telefonoId:personaResponse.getTelefonos().get(0).getTelefonoId());

				List<RespApiPersona.Correo> correosResponse = new ArrayList<>();
				for (ApiPersonaRequestDTO.Correo correoPersona : correosPersona) {
					List<CorreoApiDTO> correoRpta = correoRepositorySP.buscarEmailPersona(correoPersona.getCorreo());	
					if(correoRpta.isEmpty()){
						RespBase<ApiPersonaRequestDTO.Correo> requestCorreo = new RespBase<>();
						ApiPersonaRequestDTO.Correo correoPayload = new ApiPersonaRequestDTO.Correo(null,correoPersona.getTipoCorreo(), correoPersona.getCorreo());
						requestCorreo.setPayload(correoPayload);
						RespBase<RespApiPersona.Correo> respuesta = personaApiClient.crearCorreo(personaResponse.getPersona().getPersonaId().intValue(),requestCorreo);
						if(Boolean.TRUE.equals(respuesta.getStatus().getSuccess())) {
							correoId = respuesta.getPayload().getCorreoId();
							correosResponse.add(respuesta.getPayload());
						}else {						
							responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, respuesta.getStatus().getError().toString());
							return responseCuentaEntidad;						
						}
					}else {
						RespApiPersona.Correo correo = new RespApiPersona.Correo();
						correo.setCorreoId(correoRpta.get(0).getCorreoId());
						correo.setCorreo(correoRpta.get(0).getCorreo());
						correo.setTipoCorreo(correoRpta.get(0).getTipoCorreo());
						correo.setPersonaId(correoRpta.get(0).getPersonaId());
						correosResponse.add(correo);
						correoId=correoRpta.get(0).getCorreoId();
					}
				}					
				personaResponse.setCorreos(correosResponse);				
				List<AsignaRolRequestDTO> listaRoles = request.getPayload().getListaRoles();
				int i = 0;
				List<ValidarUsuarioDTO> obtenerUsuarioResponse = gestionRepository.ObtenerUsuarioExistente(personaResponse.getDocumentos().get(0).getNumeroDocumento());
				for (AsignaRolRequestDTO asignaRolRequestDTO : listaRoles) {
					if(asignaRolRequestDTO.getRolId() != null){											
						if(obtenerUsuarioResponse.isEmpty()) {
							if(i == 0) {
								RespBase<ApiSeguridadRequestDTO> apiUsuario = new RespBase<>();
								ApiSeguridadRequestDTO requestCreaUsuario = new ApiSeguridadRequestDTO(personaResponse.getDocumentos().get(0).getNumeroDocumento(),personaResponse.getCorreos().get(0).getCorreo(), personaResponse.getPersona().getPersonaId(), Constantes.COD_PLANTILLA_CREATE_USER_MAESTRA_TALENTO, null);
								apiUsuario.setPayload(requestCreaUsuario);
								RespBase<RespApiSeguridad> respuestaCreaUser = seguridadApiClient.registrarUsuarioEntidad(cuentaEntidad.getEntidadId(),apiUsuario);
								if(Boolean.FALSE.equals(respuestaCreaUser.getStatus().getSuccess())) {
									responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, respuestaCreaUser.getStatus().getError().toString());
									return responseCuentaEntidad;
								}else {
									usuarioId = respuestaCreaUser.getPayload().getUsuarioId();
								}	
							}
						}else {	
							for (ValidarUsuarioDTO obtenerUsuario : obtenerUsuarioResponse) {
								if(i == 0) {

									response = seguridadApiClient.asignarUsuarioEntidad(cuentaEntidad.getEntidadId(), obtenerUsuario.getUsuarioId().longValue(), Constantes.ESTADO_ACTIVO);
									if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
										responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, response.getStatus().getError().toString());
										return responseCuentaEntidad;
									}else {
										usuarioId = obtenerUsuario.getUsuarioId().longValue();
									}
									enviarCorreoExisteUsuario(personaResponse.getCorreos().get(0).getCorreo(),obtenerUsuario.getCorreoElectronico(),obtenerUsuario.getUsuario(),false);
								}
							}	
						}
						RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();

						AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
						asignaRolDTO.setRolId(asignaRolRequestDTO.getRolId());
						asignaRolDTO.setUsuarioId(usuarioId);
						requestAsignaRol.setPayload(asignaRolDTO);
						response = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
						if(Boolean.FALSE.equals(response.getStatus().getSuccess())) {
							responseCuentaEntidad = ParametrosUtil.setearResponse(responseCuentaEntidad, Boolean.FALSE, response.getStatus().getError().toString());
							return responseCuentaEntidad;
						}
						
					}else {

						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "NO SE ENCONTRO EL ROL");
					}
					i++;
				}			
				ctaEntidadAsignar.setPersonaId(personaId);			
			}	
			ctaEntidadAsignar.setTelefonoId(telefonoId);
			ctaEntidadAsignar.setCorreoId(correoId); 
			ctaEntidadAsignar.setUsuarioId(usuarioId);
			ctaEntidadAsignar.setPuestoTrabajoId(cuentaEntidad.getPuestoTrabajoId());
			ctaEntidadAsignar.setMotivo(cuentaEntidad.getMotivo());
			ctaEntidadAsignar.setCampoSegUpd(EstadoRegistro.ACTIVO.getCodigo(),token.getUsuario().getUsuario(), Instant.now());
			cuentaEntidadRepository.save(ctaEntidadAsignar);
			reasignarPayload.setCuentaEntidad(ctaEntidadAsignar);
		}
					
			return new RespBase<RespReasignarCuentaEntidad>().ok(reasignarPayload);

	}
	
	public void enviarCorreoExisteUsuario(String correo,String correoUsuario,String nroDocumento,boolean ejecutarHilo){
		Map<String, Object> parametrosCorreo = new HashMap<>();
		List<String> correosEnvio = new ArrayList<>();		
		correosEnvio.add(correo);
		if(!correosEnvio.isEmpty()) {
			parametrosCorreo.put(Constantes.CORREOS_ENVIO, correosEnvio);
			Map<String, Object> parametrosPlantilla = new HashMap<>();
			parametrosPlantilla.put("CORREO_ELECTRONICO", correoUsuario);
			parametrosPlantilla.put("NRO_DOCUMENTO", nroDocumento);
			parametrosPlantilla.put("CORREO_SERVIR", variablesSistema.correoContacto);
			parametrosCorreo.put(Constantes.PARAMETROS, parametrosPlantilla);
			notificacionService.enviarNotificacion(Constantes.ASUNTO_CREAR_SOLICITUD, Constantes.PLANTILLA_USUARIO_EXISTENTE, parametrosCorreo, ejecutarHilo);
		}
	}
	
	@Override
	public RespBase<RespComboUsuarioPorEntidadRol> listarCuentasPorEntidadPorRol(Map<String, Object> parametroMap) {
		RespComboUsuarioPorEntidadRol respPayload = new RespComboUsuarioPorEntidadRol();
		List<ObtenerCuentaEntidadRolDTO> listaCuentasPorEntidadRol = gestionRepository.listarCuentaPorEntidadPorRol(parametroMap);	
		respPayload.setItems(listaCuentasPorEntidadRol);
		return new RespBase<RespComboUsuarioPorEntidadRol>().ok(respPayload);
	}

//	
//	RespListaEntidades respPayload = new RespListaEntidades();
//	List<RespListaEntidades.Entidades> listaEntidadesByRol =  new ArrayList<>();	
//	List<ListaEntidades> listaEntidades = gestionRepository.listaEntidadesByRol(parametroMap);	
//	List<ObtenerRolDTO> listaRoles = gestionRepository.listaRolesByEntidades(parametroMap);
//	generarListaCuentaAsociadad(listaEntidades, listaRoles, listaEntidadesByRol);
//	respPayload.setListaEntidades(listaEntidadesByRol);
//	return new RespBase<RespListaEntidades>().ok(respPayload);
	
}
