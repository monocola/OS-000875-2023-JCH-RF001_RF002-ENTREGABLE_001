package pe.gob.servir.entidad.repository.impl;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;

import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.GenericResponseMessage;
import pe.gob.servir.entidad.model.TelefonoApiDTO;
import pe.gob.servir.entidad.repository.TelefonoRepositorySP;

@Repository
public class TelefonoRepositoryImpl implements TelefonoRepositorySP{

	@PersistenceContext(unitName = "entidadEntityManagerFactory", type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;
	
	
	@SuppressWarnings("unchecked")
	public List<TelefonoApiDTO> buscarTelefonoPersona(Long personaId, String telefono, String tipoTelefono){
		List<TelefonoApiDTO> lista = new ArrayList<TelefonoApiDTO>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCAR_TELEFONO,TelefonoApiDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		//storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, personaId);
		storedProcedure.setParameter(2, telefono);
		//storedProcedure.setParameter(3, tipoTelefono);
		storedProcedure.execute();
		lista  = storedProcedure.getResultList();
		return lista;
	}
	
	public TelefonoApiDTO buscarTelefonoId(Long personaId, Long telefonoId){
		TelefonoApiDTO telf = new TelefonoApiDTO();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCAR_TELEFONO,TelefonoApiDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, personaId);
		storedProcedure.setParameter(2, telefonoId);
		storedProcedure.execute();
		telf = (TelefonoApiDTO) storedProcedure.getSingleResult();
		return telf;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<GenericResponseMessage> updateTelefonoSP(Long personaId, Long telefonoId, String telefono, String tipoTelefono) {
		//TelefonoApiDTO telf = new TelefonoApiDTO();
		//GenericResponseMessage
		//StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDA_UPD_TELEFONO_PERSONA,TelefonoApiDTO.class);
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDA_UPD_TELEFONO_PERSONA,GenericResponseMessage.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, personaId);
		storedProcedure.setParameter(2, telefonoId);
		storedProcedure.setParameter(3, telefono);
		storedProcedure.setParameter(4, tipoTelefono);
		storedProcedure.execute();
		//telf = (TelefonoApiDTO) storedProcedure.getSingleResult();
		return storedProcedure.getResultList();
		//return telf;
	}
	
	@SuppressWarnings("unchecked")
	public List<TelefonoApiDTO> buscandoTelefonoPersona(String telefono){
		List<TelefonoApiDTO> lista = new ArrayList<TelefonoApiDTO>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCANDO_TELEFONO,TelefonoApiDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, telefono);
		storedProcedure.execute();
		lista  = storedProcedure.getResultList();
		return lista;
	}
}
