package pe.gob.servir.entidad.service;


import java.io.InputStream;
import java.util.List;
import java.util.Map;

import javax.validation.Valid;

import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.model.DetUnidadOrganica;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.request.ReqActualizaNuevoEvaluadorServidorCivil;
import pe.gob.servir.entidad.request.ReqActualizarPuesto;
import pe.gob.servir.entidad.request.ReqAgregarPuesto;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEditaServidorCivil;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.EditParticipanteGDRDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilExcelDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespBuscarParticipante;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadoEvaluador;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadoresServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadosServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarParticipanteServidorCivil;
import pe.gob.servir.entidad.response.RespBuscarPersonasPuestoUoServidorCivil;
import pe.gob.servir.entidad.response.RespObtenePuestoUoServidorCivil;
import pe.gob.servir.entidad.response.RespObtenerDatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface ServidorCivilService {

	RespBase<GenericResponseMessage> crearServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request, MyJsonWebToken jwt) throws ValidationException, Exception;

	RespBase<GenericResponseMessage> crearServidorCivil(@Valid ReqBase<BeanServidorCivilDTO> request, MyJsonWebToken jwt, boolean esParaMasivo) throws ValidationException, Exception;

	RespBase<Object> cesarServidorCivil(@Valid ReqBase<DetUnidadOrganica> request, MyJsonWebToken jwt);
	
	RespBase<RespObtenerDatosPersonalesServidorCivilDTO> obtenerDatosPersonalesServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespObtenePuestoUoServidorCivil> obtenerPuestoUoServidorCivil(Map<String, Object> parametroMap) throws ValidationException;

	RespBase<Object> eliminarServidorCivil(MyJsonWebToken jwt, Long detUnidadOrganicaId, String estado);
	
	RespBase<RespBuscarPersonasPuestoUoServidorCivil> buscarPersonasPuestoUoServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteServidorCivil> buscarParticipanteServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteServidorCivil> buscarParticipanteServidorCivilNoActivos(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadoresServidorCivil> buscarParticipantesEvaluadoresServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosNoMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosSinEvaluadorMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesMandoMedioServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil> buscarEvaluadosSinEvaluadoresServidorCivil(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<Object> actualizarParticiapantesNuevoEvaluadorServidorCivil(@Valid ReqBase<ReqActualizaNuevoEvaluadorServidorCivil> request, MyJsonWebToken jwt) throws ValidationException;
	
	RespBase<Object> actualizarParticiapantesEvaluadorServidorCivil(@Valid ReqBase<ReqActualizaNuevoEvaluadorServidorCivil> request, MyJsonWebToken jwt) throws ValidationException;
	
	RespBase<Object> quitarEvaluadorAsignadoUnEvaluadoServidorCivil(Map<String, Object> parametroMap, MyJsonWebToken jwt) throws ValidationException;
	
	RespBase<Object> editarServidorCivil(@Valid ReqBase<ReqEditaServidorCivil> request, MyJsonWebToken jwt) throws ValidationException;
	
	RespBase<Object> validarListaServCivil(MyJsonWebToken token, List<ServidorCivilExcelDTO> lista, Long entidadId) throws Exception ;
	
	List<ServidorCivilExcelDTO> obtenerListaServCivilfromExcel(InputStream uploadedInputStream);

	RespBase<Object> agregarPuesto(@Valid ReqBase<ReqAgregarPuesto> request, MyJsonWebToken jwt) throws ValidationException;

	RespBase<Object> actualizarDetallePuesto(@Valid ReqBase<ReqActualizarPuesto> request, MyJsonWebToken jwt) throws Exception;

	RespBase<Object> editarParticipanteGdr(@Valid ReqBase<EditParticipanteGDRDTO> request, MyJsonWebToken jwt) throws Exception;
	
	RespBase<RespBuscarParticipanteEvaluadoEvaluador> buscarEvaluadoEvaluador(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipante> buscarParticipante(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<RespBuscarParticipanteEvaluadosServidorCivil> buscarParticipantesEvaluadosPersonaServidorCivil(Map<String, Object> parametroMap) 
			throws ValidationException;

	RespBase<Object> obtenerDatosUsuarioEmail(Map<String, Object> parametroMap, MyJsonWebToken jwt);

	RespBase<RespBuscarParticipanteEvaluadosSinEvaluadoresServidorCivil> buscarEvaluadosSinEvaluadoresEntidad(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<Object> buscarParticipantesEntidad(Map<String, Object> parametroMap) throws ValidationException;
	
	RespBase<Object> buscarParticipantesEvaluadosServidorCivilEntidad(Map<String, Object> parametroMap) throws ValidationException;

	RespBase<Object> buscarParticipanteByDUOId(Map<String, Object> parametroMap) throws ValidationException;
	
}
