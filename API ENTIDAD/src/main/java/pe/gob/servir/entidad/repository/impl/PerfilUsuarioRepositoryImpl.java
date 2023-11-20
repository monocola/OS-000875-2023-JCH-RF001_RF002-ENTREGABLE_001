package pe.gob.servir.entidad.repository.impl;

import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;

import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.adapter.BeanAdapterServidorCivil;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.feign.client.PersonaApiClient;
import pe.gob.servir.entidad.feign.client.SeguridadApiClient;
import pe.gob.servir.entidad.model.PerfilUsuarioDTO;
import pe.gob.servir.entidad.repository.DetUnidadOrganicaRepository;
import pe.gob.servir.entidad.repository.PerfilUsuarioRepository;

@Repository
public class PerfilUsuarioRepositoryImpl implements PerfilUsuarioRepository {

	private static final Logger LOGGER = Logger.getLogger(PerfilUsuarioRepositoryImpl.class);

	@PersistenceContext(unitName = "entidadEntityManagerFactory", type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@Autowired
	BeanAdapterServidorCivil beanAdapter;

	@Autowired
	PersonaApiClient personaApiClient;

	@Autowired
	DetUnidadOrganicaRepository detUnidadOrganicaRepository;

	@Autowired
	SeguridadApiClient seguridadApiClient;

	@SuppressWarnings("unchecked")
	@Override
	public List<PerfilUsuarioDTO> obtenerPerfilUsuario(Map<String, Object> parametroMap) throws ValidationException {
		List<PerfilUsuarioDTO> lista;
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_GENERAL + "." + Constantes.SP_OBTENER_PERFIL_USUARIO,
				PerfilUsuarioDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

}