package pe.gob.servir.entidad.repository.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;

import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.EstadoSolicitud;
import pe.gob.servir.entidad.model.ObtenerSolicitud;
import pe.gob.servir.entidad.repository.SolicitudRepositorySP;

@Repository
public class SolicitudRepositoryImpl implements SolicitudRepositorySP{
	
	@PersistenceContext(unitName = "entidadEntityManagerFactory",type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ObtenerSolicitud> buscarSolicitudFilter(Map<String, Object> parametroMap) {
		List<ObtenerSolicitud> lista = new ArrayList<ObtenerSolicitud>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.BUSCAR_ADMINISTRADORES_FILTRO,ObtenerSolicitud.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Date.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Date.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(8, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(9, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(10, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(11, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("razonSocial"));
		storedProcedure.setParameter(2, parametroMap.get("ruc"));
		storedProcedure.setParameter(3, parametroMap.get("sector"));
		storedProcedure.setParameter(4, parametroMap.get("nivelGobierno"));
		storedProcedure.setParameter(5, parametroMap.get("fechaInicio"));
		storedProcedure.setParameter(6, parametroMap.get("fechaFin"));
		storedProcedure.setParameter(7, parametroMap.get("estado"));
		storedProcedure.setParameter(8, parametroMap.get("departamento"));
		storedProcedure.setParameter(9, parametroMap.get("provincia"));
		storedProcedure.setParameter(10, parametroMap.get("distrito"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<EstadoSolicitud> buscarSolicitudByNro(String numeroRuc) {
		List<EstadoSolicitud> lista = new ArrayList<EstadoSolicitud>();
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCA_SOLICITUD_BY_NRO,EstadoSolicitud.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, numeroRuc);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	
}
