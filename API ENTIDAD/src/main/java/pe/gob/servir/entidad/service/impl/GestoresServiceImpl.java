package pe.gob.servir.entidad.service.impl;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.adapter.BeanAdapterGestores;
import pe.gob.servir.entidad.api.dto.ApiActualizarEstadoRolUsuario;
import pe.gob.servir.entidad.api.dto.ApiActualizarPersonaNatural;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.CorreoApiDTO;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.GestorDTO;
import pe.gob.servir.entidad.model.Gestores;
import pe.gob.servir.entidad.model.ListaGestorDTO;
import pe.gob.servir.entidad.model.TelefonoApiDTO;
import pe.gob.servir.entidad.model.UserRolEntidadDTO;
import pe.gob.servir.entidad.repository.CorreoRepositorySP;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.GestoresRepository;
import pe.gob.servir.entidad.repository.TelefonoRepositorySP;
import pe.gob.servir.entidad.repository.UsuarioRepositorySP;
import pe.gob.servir.entidad.request.ReqActualizaGestorORH;
import pe.gob.servir.entidad.request.ReqGestorORH;
import pe.gob.servir.entidad.response.RespApiObtenerUsuario;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespGestores;
import pe.gob.servir.entidad.response.RespListaGestorOrh;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.GestoresService;
import pe.gob.servir.entidad.service.PersonaService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class GestoresServiceImpl implements GestoresService{
	private static final Logger LOGGER = Logger.getLogger(GestoresServiceImpl.class);
	@Autowired
    private GestionRepository gestionRepository;
	
	@Autowired
	private BeanAdapterGestores beanAdapterGestores;
	
	@Autowired
	private PersonaService personaService;
	
	@Autowired
	VariablesSistema variablesSistema;
	
	@Autowired
	private SeguridadApiClient seguridadApiClient;
	
	@Autowired
	private UsuarioRepositorySP usuarioRepositorySP;
	
	@Autowired
	private GestoresRepository gestoresRepository;
	
	@Autowired
	PersonaApiClient personaApiClient;

	@Autowired
	CorreoRepositorySP correoRepositorySP;
	
	@Autowired
	TelefonoRepositorySP telefonoRepositorySP;
	
	@Override
	public RespBase<Object> listaGestoresOrh(Long entidadId) {
		List<ListaGestorDTO> listaGestor = gestionRepository.listarGestoresORH(entidadId);
		RespListaGestorOrh respPayload = new RespListaGestorOrh();
		respPayload.setGestores(listaGestor);
		return new RespBase<Object>().ok(respPayload);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public RespBase<RespGestores> registrarGestores(ReqGestorORH request, MyJsonWebToken token) {
		RespBase<RespGestores> response = new RespBase<>();
		PersonaDTO persona = beanAdapterGestores.adapToPersonaDTO(request);
		if (request.getTipoDocumento().intValue() == variablesSistema.tipoDocumentoDni) {
			persona.setPaisId(variablesSistema.idPaisPeru);
		} else {
			persona.setPaisId(request.getPaisId());
		}
		RespBase<ApiPersonaRequestDTO> personaNaturalResquest = new RespBase<>();
		ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersonaNatural = new ApiPersonaRequestDTO<>();
		ParametrosUtil.setearPersonaNatural(apiPersonaNatural, persona);
		personaNaturalResquest.setPayload(apiPersonaNatural);
		RespBase<Object> responseWS = personaService.obtenerInsertarPersona(
				variablesSistema.tipoPersonaNatural, persona.getTipoDocumento(),
				persona.getNumeroDocumento(), personaNaturalResquest);
		if (Boolean.FALSE.equals(responseWS.getStatus().getSuccess())) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					responseWS.getStatus().getError().getMessages().get(0).toString());
			return response;
		}
		RespApiPersona personaResponse = (RespApiPersona) responseWS.getPayload();
		String nroDocumento = personaResponse.getDocumentos().get(0).getNumeroDocumento();
		RespBase<RespApiObtenerUsuario> rptaUsuario = seguridadApiClient.buscarUsuariosByFiltro(nroDocumento, null, null, Constantes.ESTADO_ACTIVO);
		if (Boolean.FALSE.equals(rptaUsuario.getStatus().getSuccess())) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					rptaUsuario.getStatus().getError().toString());
			return response;
		}
		response = beanAdapterGestores.adapToUsuarios(rptaUsuario,nroDocumento,personaResponse,request);		
		
		if (!usuarioRepositorySP.existeRolUsuario(response.getPayload().getGestores().getUsuarioId(),request.getRolId())) {
			RespBase<AsignaRolRequestDTO> requestAsignaRol = new RespBase<>();
			AsignaRolRequestDTO asignaRolDTO = new AsignaRolRequestDTO();
			asignaRolDTO.setRolId(request.getRolId());
			asignaRolDTO.setEntidadId(request.getEntidadId());
			asignaRolDTO.setUsuarioId(response.getPayload().getGestores().getUsuarioId());
			requestAsignaRol.setPayload(asignaRolDTO);
			RespBase<Object> responseSeguridad = seguridadApiClient.asignarRolUsuario(requestAsignaRol);
			if (Boolean.FALSE.equals(responseSeguridad.getStatus().getSuccess())) {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						responseSeguridad.getStatus().getError().toString());
				return response;
			}
		}
		RespGestores respGestor = new RespGestores();
		Gestores gestorOptional = new Gestores();
		gestorOptional.setPersonaId(response.getPayload().getGestores().getPersonaId());		
		gestorOptional.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
		Example<Gestores> oGestor = Example.of(gestorOptional);
		List<Gestores> gestorExiste = gestoresRepository.findAll(oGestor);
		if(gestorExiste.size()==0 || gestorExiste.isEmpty()) {		
			Gestores gestor = new Gestores();
			gestor.setEntidadId(request.getEntidadId());
			gestor.setRolId(request.getRolId());
			gestor.setPersonaId(response.getPayload().getGestores().getPersonaId());
			gestor.setUsuarioId(response.getPayload().getGestores().getUsuarioId());
			gestor.setCorreoId(response.getPayload().getGestores().getCorreoId());
			gestor.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			gestor=gestoresRepository.save(gestor);
			respGestor.setGestores(gestor);
			response.setPayload(respGestor);
			response.getStatus().setSuccess(Boolean.TRUE);
		}else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					"La persona ya esta registrado como gestor ORH");
		}
		return response;
	}

	@Override
	public RespBase<GestorDTO> getGestorId(Long gestorId) {
		LOGGER.info("Metodo getSolicitudExternaId...");
		RespBase<GestorDTO> response = new RespBase<GestorDTO>();
		List<GestorDTO> gestor = this.gestionRepository.listaGestorORHId(gestorId);
		GestorDTO gestorDTO;
		if ( !gestor.isEmpty() || gestor.size() > 0) {
			gestorDTO = this.beanAdapterGestores.adapToListaGestorDTO(gestor);
			response.getStatus().setSuccess(true);
		}else {
			gestorDTO = null;
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add("No se encontro gestor ORH");
		}
		response.setPayload(gestorDTO);
		return response;
	}
	
	@Override
	public RespBase<RespGestores> actualizaGestores(ReqActualizaGestorORH request, MyJsonWebToken token) {
		LOGGER.info("Metodo actualizaGestores...");
		RespBase<RespGestores> response = new RespBase<>();
		Optional<Gestores> gestores = gestoresRepository.findById(request.getGestorId());
		if (!gestores.isPresent()) {
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add("No se encontro el Gestor");
			return response;	
		}
		Gestores gestor = new Gestores();
		int validaCorreo = 0;
		int validaTelefono = 0;
		int validaTelefonoId = 0;
		int validaCelular = 0;
		RespBase<RespApiPersona.Telefono> telefonoC = new RespBase<RespApiPersona.Telefono>();
		RespBase<RespApiPersona.Telefono> celularC = new RespBase<RespApiPersona.Telefono>();
		
		PersonaDTO persona = beanAdapterGestores.adapToUPDPersonaDTO(request);
		if (request.getTipoDocumento().intValue() == variablesSistema.tipoDocumentoDni) {
			persona.setPaisId(variablesSistema.idPaisPeru);
		} else {
			persona.setPaisId(request.getPaisId());
		}
		
		LOGGER.info("Busca ApiPersona...");
		RespBase<RespApiPersona> personaWS = personaApiClient.obtienePersonaPorDocumento(persona.getTipoDocumento(), persona.getNumeroDocumento());
		if (personaWS.getStatus().getSuccess()) {
			if (personaWS.getPayload() == null || personaWS.getPayload().getPersona() == null) {
				personaWS.getStatus().setSuccess(Boolean.FALSE);
				personaWS.getStatus().getError().getMessages().add("No se encontro la persona registrada");
				return response;
				
			}else {
				if( request.getFechaNacimiento() != null) {
					if(persona.getTipoDocumento() != 1 || persona.getTipoDocumento() != 6 ) {
						LOGGER.info("valida y actualiza fecha nacimiento...");
						RespBase<RespApiPersona> personaNatu = new RespBase<RespApiPersona>();
						RespBase<ApiActualizarPersonaNatural> persNatural = new RespBase<ApiActualizarPersonaNatural>();
						ApiActualizarPersonaNatural peNatural = new ApiActualizarPersonaNatural();
						peNatural.setPersonaId(request.getPersonaId());
						peNatural.setFechaNacimiento(request.getFechaNacimiento());
						persNatural.setPayload(peNatural);
						personaNatu = personaApiClient.actualizaPersonaNatural(request.getPersonaId(), persNatural);
						
						if (personaNatu.getPayload() == null || personaNatu.getPayload().getPersona() == null) {
							personaNatu.getStatus().setSuccess(Boolean.FALSE);
							personaNatu.getStatus().getError().getMessages().add("Error al actualizar la fecha de nacimiento");
							return response;
						} else {
							
						}
					}
				}
			
				List<CorreoApiDTO> correoRpta = correoRepositorySP.buscarEmailPersona(request.getCorreo());
				if(!correoRpta.isEmpty()) {
					for (int i = 0; i < correoRpta.size(); i++) {
						if (correoRpta.get(i).getCorreo().equals(request.getCorreo()) && correoRpta.get(i).getCorreoId().equals(request.getCorreoId())) {
							validaCorreo = 1;
						}
					}
					if(validaCorreo != 1) {
						response.getStatus().setSuccess(Boolean.FALSE);
						response.getStatus().getError().getMessages().add("dirección de correo ya existe");
						return response;	
					}
				}
				
				if (!request.getPersonaId().equals(null) ) {
					List<TelefonoApiDTO> telefonoRpta = telefonoRepositorySP.buscarTelefonoPersona(request.getPersonaId(), request.getTelefono(), Constantes.TIPO_TELEFONO_CASA);
					if(!telefonoRpta.isEmpty()) {
						for (int i = 0; i < telefonoRpta.size(); i++) {
							if(telefonoRpta.get(i).getTelefono().equals(request.getTelefono())) {
								validaTelefono = 1;
							}
							if (telefonoRpta.get(i).getTelefonoId().equals(request.getTelefonoId())) {
								validaTelefonoId = 1;
							}
						}
						if(validaTelefono != 1) {
							response.getStatus().setSuccess(Boolean.FALSE);
							response.getStatus().getError().getMessages().add("telefono ya existe");
							return response;
						}
					}	
				}
				
				if( null != request.getCelular()) {
					List<TelefonoApiDTO> celularRpta = telefonoRepositorySP.buscarTelefonoPersona(request.getPersonaId(), request.getCelular(), Constantes.TIPO_TELEFONO_CELULAR);
					if(!celularRpta.isEmpty()) {
						for (int i = 0; i < celularRpta.size(); i++) {
							if (celularRpta.get(i).getTelefono().equals(request.getTelefono()) || celularRpta.get(i).getTelefonoId().equals(request.getTelefonoId())) {
								validaCelular = 1;
							}
						}
						if(validaCelular != 1) {
							response.getStatus().setSuccess(Boolean.FALSE);
							response.getStatus().getError().getMessages().add("Celular ya existe");
							return response;	
						}
					}
				}
				
				
				if (!personaWS.getPayload().getCorreos().isEmpty()) {
					LOGGER.info("Busca Correo...");
					if(validaCorreo != 1) {
						RespBase<RespApiPersona.Correo> correoC = new RespBase<RespApiPersona.Correo>();
						// Update Correo
						ApiPersonaRequestDTO.Correo correo = new ApiPersonaRequestDTO.Correo();
						RespBase<ApiPersonaRequestDTO.Correo> apiCorreo = new RespBase<ApiPersonaRequestDTO.Correo>();
						
						for (int i = 0; i < personaWS.getPayload().getCorreos().size(); i++) {
							if (personaWS.getPayload().getCorreos().get(i).getCorreoId().equals(request.getCorreoId())) {
								correo.setCorreoId(request.getCorreoId());
								correo.setCorreo(request.getCorreo());
								correo.setTipoCorreo(personaWS.getPayload().getCorreos().get(i).getTipoCorreo());
								apiCorreo.setPayload(correo);
								correoC = personaApiClient.actualizaCorreo(request.getCorreoId().toString(), apiCorreo);
							}
						}
						
						if (Boolean.FALSE.equals(correoC.getStatus().getSuccess())) {
							LOGGER.info("Error Correo...");
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
									correoC.getStatus().getError().toString());
							return response;
						}
					}
				}
				
				
				//Inserta o Actualiza Telefono
				LOGGER.info("Busca Telefono...");
				List<GenericResponseMessage> responseGenericTelf = null;
				List<TelefonoApiDTO> telefonoRpta = telefonoRepositorySP.buscarTelefonoPersona(request.getPersonaId(), request.getTelefono(), Constantes.TIPO_TELEFONO_CASA);
				ApiPersonaRequestDTO.Telefono telefono = new ApiPersonaRequestDTO.Telefono();
				RespBase<ApiPersonaRequestDTO.Telefono> apiTelefono = new RespBase<ApiPersonaRequestDTO.Telefono>();
				if(!telefonoRpta.isEmpty()) {
					for (int i = 0; i < telefonoRpta.size(); i++) {
						
						if (!(request.getTelefonoId() == null)) {
							telefono.setTelefonoId(request.getTelefonoId());
							telefono.setNumeroTelefono(request.getTelefono());
							telefono.setTipoTelefono(Constantes.TIPO_TELEFONO_CASA);
							telefono.setNumeroAnexo(request.getAnexo());
							gestor.setTelefonoId(request.getTelefonoId());
							apiTelefono.setPayload(telefono);
							responseGenericTelf = telefonoRepositorySP.updateTelefonoSP(request.getPersonaId(), telefono.getTelefonoId(), telefono.getNumeroTelefono(), telefono.getTipoTelefono());
							if (responseGenericTelf.get(0).getCodigo() == 0) {
								response = ParametrosUtil.setearResponse(response, Boolean.FALSE, responseGenericTelf.get(0).getMensaje());
								return response;
							}
							telefonoC = personaApiClient.actualizaTelefono(request.getTelefonoId().toString(), apiTelefono);
							if (!telefonoC.getStatus().getSuccess()) {
								response = ParametrosUtil.setearResponse(response, Boolean.FALSE, telefonoC.getStatus().getError().getMessages().get(0));
								return response;
							}
							
						} else {
							if (request.getTelefonoId() == null) {
								if (!(request.getTelefono() == null) ) {
									telefono.setNumeroTelefono(request.getTelefono());
									telefono.setTipoTelefono(Constantes.TIPO_TELEFONO_CASA);
									telefono.setNumeroAnexo(request.getAnexo());	
									apiTelefono.setPayload(telefono);
									gestor.setTelefonoId(telefonoC.getPayload().getTelefonoId());
									telefonoC = personaApiClient.crearTelefono(request.getPersonaId().intValue(), apiTelefono);
									
									if(!telefonoC.getStatus().getSuccess()) {
										response = ParametrosUtil.setearResponse(response, Boolean.FALSE, telefonoC.getStatus().getError().getMessages().get(0));
										return response;
									}
								}
							}
						}
					}
				} else {
					if (!(request.getTelefonoId() == null)) {
						telefono.setTelefonoId(request.getTelefonoId());
						telefono.setNumeroTelefono(request.getTelefono());
						telefono.setTipoTelefono(Constantes.TIPO_TELEFONO_CASA);
						//telefono.setNumeroAnexo(request.getAnexo());
						gestor.setTelefonoId(request.getTelefonoId());
						apiTelefono.setPayload(telefono);
						String telefonoUPT = telefono.getNumeroTelefono() == null ? null : telefono.getNumeroTelefono().length() == 0 ? null : telefono.getNumeroTelefono(); 
						responseGenericTelf = telefonoRepositorySP.updateTelefonoSP(request.getPersonaId(), telefono.getTelefonoId(), telefonoUPT, telefono.getTipoTelefono());
						if (responseGenericTelf.get(0).getCodigo() == 0) {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE, responseGenericTelf.get(0).getMensaje());
							return response;
						}
						telefonoC = personaApiClient.actualizaTelefono(request.getTelefonoId().toString(), apiTelefono);
						if (telefonoC.getStatus().getSuccess()) {
							response = ParametrosUtil.setearResponse(response, Boolean.FALSE, telefonoC.getStatus().getError().getMessages().get(0));
							return response;
						}
						
					} else {
						if (request.getTelefonoId() == null) {
							if (!(request.getTelefono() == null) ) {
								telefono.setNumeroTelefono(request.getTelefono());
								telefono.setTipoTelefono(Constantes.TIPO_TELEFONO_CASA);
								//telefono.setNumeroAnexo(request.getAnexo());	
								apiTelefono.setPayload(telefono);
								
								telefonoC = personaApiClient.crearTelefono(request.getPersonaId().intValue(), apiTelefono);
								
								if(!telefonoC.getStatus().getSuccess()) {
									response = ParametrosUtil.setearResponse(response, Boolean.FALSE, telefonoC.getStatus().getError().getMessages().get(0));
									return response;
								}
								gestor.setTelefonoId(telefonoC.getPayload().getTelefonoId());
							}
						}
					}
				}
				
				
				
				//Inserta o Actualiza Celular
				//LOGGER.info("Busca Celular...");
				// List<GenericResponseMessage> responseGenericCel = null;
				// List<TelefonoApiDTO> celularRpta = telefonoRepositorySP.buscarTelefonoPersona(request.getPersonaId(), request.getCelular(), Constantes.TIPO_TELEFONO_CELULAR);
				// ApiPersonaRequestDTO.Telefono celular = new ApiPersonaRequestDTO.Telefono();
				// RespBase<ApiPersonaRequestDTO.Telefono> apiCelular = new RespBase<ApiPersonaRequestDTO.Telefono>();
				// if(!celularRpta.isEmpty()) {
				// 	for (int i = 0; i < celularRpta.size(); i++) {
						
				// 		if (!(request.getCelularId() == null)) {
				// 			celular.setTelefonoId(request.getCelularId());
				// 			celular.setNumeroTelefono(request.getCelular());
				// 			celular.setTipoTelefono(Constantes.TIPO_TELEFONO_CELULAR);
				// 			celular.setNumeroAnexo(request.getTelefono() == null ? request.getAnexo() : null);
							
				// 			gestor.setCelularId(request.getCelularId());
				// 			responseGenericCel = telefonoRepositorySP.updateTelefonoId(request.getPersonaId(), celular.getTelefonoId(), celular.getNumeroTelefono(), celular.getTipoTelefono());
							
				// 			if (responseGenericCel.get(0).getCodigo() == 0) {
				// 				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, responseGenericCel.get(0).getMensaje());
				// 				return response;
				// 			}
							
				// 		} else {
				// 			if (request.getCelularId() == null) {
				// 				if (!(request.getCelular() == null) ) {
				// 					celular.setNumeroTelefono(request.getCelular());
				// 					celular.setTipoTelefono(Constantes.TIPO_TELEFONO_CELULAR);
				// 					celular.setNumeroAnexo(request.getTelefono() == null ? request.getAnexo() : null);	
				// 					apiCelular.setPayload(celular);
				// 					gestor.setTelefonoId(celularC.getPayload().getTelefonoId());
				// 					celularC = personaApiClient.crearTelefono(request.getPersonaId().intValue(), apiTelefono);
									
				// 					if(!telefonoC.getStatus().getSuccess()) {
				// 						response = ParametrosUtil.setearResponse(response, Boolean.FALSE, telefonoC.getStatus().getError().getMessages().get(0));
				// 						return response;
				// 					}
				// 				}
				// 			}
				// 		}
				// 	}
				// }else {
				// 	if (request.getCelularId() == null) {
				// 		if (!(request.getCelular() == null) ) {
				// 			celular.setNumeroTelefono(request.getCelular());
				// 			celular.setTipoTelefono(Constantes.TIPO_TELEFONO_CELULAR);
				// 			celular.setNumeroAnexo(request.getTelefono() == null ? request.getAnexo() : null);	
				// 			apiCelular.setPayload(celular);
				// 			gestor.setTelefonoId(celularC.getPayload().getTelefonoId());
				// 			celularC = personaApiClient.crearTelefono(request.getPersonaId().intValue(), apiTelefono);
							
				// 			if(!telefonoC.getStatus().getSuccess()) {
				// 				response = ParametrosUtil.setearResponse(response, Boolean.FALSE, telefonoC.getStatus().getError().getMessages().get(0));
				// 				return response;
				// 			}
				// 		}
				// 	}
				// }
				
					
				
				LOGGER.info("Busca ApiSeguridad...");
				List<UserRolEntidadDTO> userRolEntidad = usuarioRepositorySP.buscarRolUsuarioEntidad(request.getUsuarioId(), request.getRolId(), request.getEntidadId()); 
				if (userRolEntidad.size() > 0) {
					RespBase<ApiActualizarEstadoRolUsuario> paramSeguridad = new RespBase<ApiActualizarEstadoRolUsuario>();
					ApiActualizarEstadoRolUsuario seguridad = new ApiActualizarEstadoRolUsuario();
					seguridad.setUsuarioRolId(userRolEntidad.get(0).getUsuarioRolId());
					seguridad.setEstado(request.getFlagUPDT().toString());
					paramSeguridad.setPayload(seguridad);
					RespBase<Object> responseSeg = seguridadApiClient.actualizarEstadoRolUsuario(paramSeguridad.getPayload().getUsuarioRolId(), paramSeguridad);
					
					if (Boolean.FALSE.equals(responseSeg.getStatus().getSuccess())) {
						response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
								responseSeg.getStatus().getError().toString());
						return response;
					}
				}
				
				LOGGER.info("Actualiza Gestor...");
				RespGestores respGestor = new RespGestores();
				gestor.setGestorId(gestores.get().getGestorId());
				gestor.setPersonaId(gestores.get().getPersonaId());
				gestor.setUsuarioId(gestores.get().getUsuarioId());
				gestor.setRolId(gestores.get().getRolId());
				gestor.setEntidadId(gestores.get().getEntidadId());
				gestor.setCorreoId(gestores.get().getCorreoId());
				
				//actualiza Telefono
				LOGGER.info("Setea Celular...");
				gestor.setCelularId(request.getCelularId() == null ? null : request.getCelularId());
				gestor.setCelular(request.getCelular() == null ? null : request.getCelular());
				gestor.setTipoCelular(request.getCelular() == null ? null : Constantes.TIPO_TELEFONO_CELULAR);
				gestor.setAnexoTelefono(request.getTelefono() != null ? request.getAnexo() : request.getCelular() != null ? request.getCelular() : null);
				
				if(request.getFlagUPDT() == 1) {
					gestor.setEstadoRegistro(request.getFlagUPDT().toString());	
				}else {
					gestor.setEstadoRegistro("2");
				}
				gestor.setCampoSegUpd(gestor.getEstadoRegistro(), token.getUsuario().getUsuario(), Instant.now());
				gestor=gestoresRepository.save(gestor);
				respGestor.setGestores(gestor);
				response.setPayload(respGestor);
				response.getStatus().setSuccess(Boolean.TRUE);
				
			}
		}
		return response;
	}

}