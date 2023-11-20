package pe.gob.servir.entidad.repository.impl;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;
import javax.validation.Valid;

import org.apache.commons.lang3.StringUtils;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import feign.FeignException;
import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.CorreoPersonaDTO;
import pe.gob.servir.entidad.model.DatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.model.DetalleUoDTO;
import pe.gob.servir.entidad.model.EmpleadoDTO;
import pe.gob.servir.entidad.model.EvaluadorEvaluadoDTO;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.ListaPuestoDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadoresServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteEvaluadosServidorCivilDTO;
import pe.gob.servir.entidad.model.ParticipanteServidorCivilDTO;
import pe.gob.servir.entidad.model.PersonasPuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.model.PuestoDTO;
import pe.gob.servir.entidad.model.PuestoUoServidorCivilDTO;
import pe.gob.servir.entidad.repository.DetUnidadOrganicaRepository;
import pe.gob.servir.entidad.repository.PuestoRepository;
import pe.gob.servir.entidad.repository.PuestoRepository2;
import pe.gob.servir.entidad.repository.ServidoresCivilRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilGDRDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Repository
public class PuestoRepositoryImpl implements PuestoRepository2 {

	private static final Logger LOGGER = Logger.getLogger(PuestoRepositoryImpl.class);


	@PersistenceContext(unitName = "entidadEntityManagerFactory",type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaPuestoDTO> listaPuesto(Map<String, Object> parametroMap) throws ValidationException {
		List<ListaPuestoDTO> result = null;
		
		try{
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+ "." + Constantes.SP_LISTAR_PUESTOS, 
				ListaPuestoDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("esJefe"));
		storedProcedure.setParameter(2, parametroMap.get("unidadOrganicaID"));
		storedProcedure.setParameter(3, parametroMap.get("nombrePuesto"));
		storedProcedure.setParameter(4, parametroMap.get("entidadId"));
		storedProcedure.setParameter(5, parametroMap.get("puestoId"));
		storedProcedure.execute();
		result =  storedProcedure.getResultList();
		}
		catch (NoResultException nre){
			
			}
		return result;
	}
	
}
