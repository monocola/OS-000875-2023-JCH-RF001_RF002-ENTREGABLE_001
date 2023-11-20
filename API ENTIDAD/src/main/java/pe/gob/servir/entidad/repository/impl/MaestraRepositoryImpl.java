package pe.gob.servir.entidad.repository.impl;

import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;

import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.ListaMaestrasDTO;
import pe.gob.servir.entidad.repository.MaestraRepository;


@Repository
public class MaestraRepositoryImpl implements MaestraRepository{ 
	
	@PersistenceContext(unitName = "entidadEntityManagerFactory",type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaMaestrasDTO> obtenerParametro(Map<String, Object> parametroMap) {
		List<ListaMaestrasDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_OBTENER_PARAMETROS,ListaMaestrasDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("tipoParametro"));
		storedProcedure.setParameter(2, parametroMap.get("estadoRegistro"));
		storedProcedure.setParameter(3, parametroMap.get("codigoNumero"));
		storedProcedure.execute();
		
		System.out.println("p1 >> " + parametroMap.get("tipoParametro"));
		System.out.println("p2 >> " + parametroMap.get("estadoRegistro"));
		System.out.println("p3 >> " + parametroMap.get("codigoNumero"));
		lista = storedProcedure.getResultList();
		return lista;
	}

	
}