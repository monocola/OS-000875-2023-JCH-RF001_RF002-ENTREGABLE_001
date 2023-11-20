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
import pe.gob.servir.entidad.model.CuentaEntidadDTO;
import pe.gob.servir.entidad.model.PaisesDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.repository.GeneralRepository;

@Repository
public class GeneralRepositoryImpl implements GeneralRepository{
	
	@PersistenceContext(unitName = "entidadEntityManagerFactory",type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<Parametro> buscarListaParametro(Integer parametroId, String tipoParametro, String codigoTexto) {
		List<Parametro> lista = new ArrayList<Parametro>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCAR_PARAMETRO,Parametro.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroId);
		storedProcedure.setParameter(2, tipoParametro);		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Parametro buscarParametro(Integer parametroId, String tipoParametro, String codigoTexto) {
		List<Parametro> lista = new ArrayList<Parametro>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCAR_PARAMETRO,Parametro.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroId);
		storedProcedure.setParameter(2, tipoParametro);		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		Parametro parametro = new Parametro();
		if(codigoTexto!=null) {	 
			for (int i = 0; i < lista.size(); i++) {
				if(lista.get(i).getCodigoTexto()!=null) {
					if(lista.get(i).getCodigoTexto().equals(codigoTexto)) {
					parametro= lista.get(i);
				}
				}
				
			}
		}else {
			 
			parametro=lista.get(0);
		} 
		return parametro;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<CuentaEntidadDTO> buscarCuentaEntidad(Long entidadId) {
		List<CuentaEntidadDTO> lista = new ArrayList<CuentaEntidadDTO>();
		StoredProcedureQuery storedProcedure = entityManager
				.createStoredProcedureQuery(Constantes.PKG_GENERAL + "." + Constantes.SP_BUSCAR_CUENTA_CLIENTE, CuentaEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<PaisesDTO> buscarPaises() {
		List<PaisesDTO> lista = new ArrayList<PaisesDTO>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GENERAL+"."+Constantes.SP_BUSCAR_PAISES,PaisesDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	
}
