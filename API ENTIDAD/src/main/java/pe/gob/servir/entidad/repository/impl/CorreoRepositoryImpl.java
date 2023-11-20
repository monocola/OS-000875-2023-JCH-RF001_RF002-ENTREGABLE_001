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
import pe.gob.servir.entidad.model.CorreoApiDTO;
import pe.gob.servir.entidad.repository.CorreoRepositorySP;

@Repository
public class CorreoRepositoryImpl implements CorreoRepositorySP{
	
	@PersistenceContext(unitName = "entidadEntityManagerFactory",type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<CorreoApiDTO> buscarEmailPersona(String correo) {
		List<CorreoApiDTO> lista = new ArrayList<CorreoApiDTO>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCAR_EMAIL,CorreoApiDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);		
		storedProcedure.setParameter(1, correo);
		storedProcedure.execute();
		lista  = storedProcedure.getResultList();
		return lista;
	}
}
