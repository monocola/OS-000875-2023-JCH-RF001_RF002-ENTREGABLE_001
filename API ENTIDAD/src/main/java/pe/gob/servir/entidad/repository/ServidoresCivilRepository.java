package pe.gob.servir.entidad.repository;


import java.util.List;
import java.util.Map;

import javax.validation.Valid;

import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.model.CorreoPersonaDTO;
import pe.gob.servir.entidad.model.DatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.model.EvaluadorEvaluadoDTO;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.ParticipanteDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadoresServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadosServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteServidorCivilDTO;
import pe.gob.servir.entidad.model.PersonasPuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.PuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO;
import pe.gob.servir.entidad.model.RespBuscarParticipantesEntidadServidorCivilDTO;
import pe.gob.servir.entidad.model.RespParticipanteDTO;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface ServidoresCivilRepository {

	RespBase<GenericResponseMessage> insertarServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request, MyJsonWebToken jwt) throws ValidationException, Exception;
	
	RespBase<GenericResponseMessage> insertarServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request, MyJsonWebToken jwt, boolean esParaMasivo) throws ValidationException, Exception;
	
	List<DatosPersonalesServidorCivilDTO> obtenerDatosPersonalesServidorCivil(Map<String, Object> parametroMap) throws ValidationException;

	List<PuestoUoServidorCivilDTO> obtenerPuestoUoServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<PersonasPuestoUoServidorCivilDTO> buscarPersonasPuestoUoServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteServidorCivilDTO> buscarParticipanteServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadoresServidorCivilDTO> buscarParticipantesEvaluadoresServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosNoMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadosServidorCivilDTO> buscarEvaluadosSinEvaluadoresServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	EvaluadorEvaluadoDTO buscarEvaluadorEvaluado(Map<String, Object> parametroMap) throws ValidationException;
	
	ParticipanteDTO buscarParticipante(Map<String, Object> parametroMap) throws ValidationException;
	
	List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadosPersonaServidorCivil(Map<String, Object> parametroMap) throws ValidationException;

	List<CorreoPersonaDTO> buscarCorreoPersona(Long personaId);
	
	List<CorreoPersonaDTO> buscarCorreoPersonaGestor(Long entidadId, Long tipoGestorid);

	List<ParticipanteEvaluadosServidorCivilDTO> buscarParticipantesEvaluadoresServidorCivilEntidad(Map<String, Object> parametroMap) throws ValidationException;
	
	List<RespBuscarParticipantesEntidadServidorCivilDTO> buscarParticipantesServidorCivilEntidad(Map<String, Object> parametroMap) throws ValidationException;
	
	List<RespBuscarParticipanteEvaluadosEntidadServidorCivilDTO> buscarParticipantesEvaluadosServidorCivilEntidad(Map<String, Object> parametroMap) throws ValidationException;

	List<RespParticipanteDTO> buscarParticipanteByDUOId(Map<String, Object> parametroMap) throws ValidationException;

    List<ParticipanteServidorCivilDTO> buscarParticipanteServidorCivilNoActivos(Map<String, Object> parametroMap) throws ValidationException;
	
}
