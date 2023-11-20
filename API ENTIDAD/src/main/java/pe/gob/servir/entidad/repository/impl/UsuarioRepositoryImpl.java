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
import pe.gob.servir.entidad.model.CuentaEntidadRolDTO;
import pe.gob.servir.entidad.model.ListaUsuarioRolEntidadDTO;
import pe.gob.servir.entidad.model.UserRolDTO;
import pe.gob.servir.entidad.model.UserRolEntidadDTO;
import pe.gob.servir.entidad.model.UsuarioRolDTO;
import pe.gob.servir.entidad.model.UsuarioRolEntidadPersonaDTO;
import pe.gob.servir.entidad.repository.UsuarioRepositorySP;

@Repository
public class UsuarioRepositoryImpl implements UsuarioRepositorySP {

	@PersistenceContext(unitName = "entidadEntityManagerFactory", type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public boolean existeRolUsuario(Long usuarioId, Long rolId) {
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDAR_ROL_USUARIO, UsuarioRolDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, usuarioId);
		storedProcedure.setParameter(2, rolId);
		storedProcedure.execute();
		List<UsuarioRolDTO> listaUsuario = storedProcedure.getResultList();
		Long usuarioRolId = listaUsuario.size() > 0 ? listaUsuario.get(0).getUsuarioRolId() : null;
		return usuarioRolId != null ? true : false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public UserRolDTO buscarRolUsuario(Long personaId, Long rolId) {
		UserRolDTO usuarioRol = null;
		StoredProcedureQuery storedProcedure = entityManager
				.createStoredProcedureQuery(Constantes.PKG_GENERAL + "." + Constantes.SP_ROL_USUARIO, UserRolDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, personaId);
		storedProcedure.setParameter(2, rolId);
		storedProcedure.execute();
		List<UserRolDTO> listaUsuario = storedProcedure.getResultList();
		usuarioRol = listaUsuario.size() > 0 ? listaUsuario.get(0) : null;
		return usuarioRol;
	}
	
	
	@SuppressWarnings("unchecked")
	@Override
	public List<CuentaEntidadRolDTO> buscarRolCuentaEntidad(Long entidadId, Long rolId) {
		List<CuentaEntidadRolDTO> lista = new ArrayList<CuentaEntidadRolDTO>();
		StoredProcedureQuery storedProcedure = entityManager
				.createStoredProcedureQuery(Constantes.PKG_GENERAL + "." + Constantes.SP_BUSCAR_ROL_CUENTA, CuentaEntidadRolDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.setParameter(2, rolId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<UserRolEntidadDTO> buscarRolUsuarioEntidad(Long usuarioId, Long rolId, Long entidadId) {
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDAR_ROL_USUARIO_ENTIDAD, UserRolEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, usuarioId);
		storedProcedure.setParameter(2, rolId);
		storedProcedure.setParameter(3, entidadId);
		storedProcedure.execute();
		List<UserRolEntidadDTO> listaUsuario = storedProcedure.getResultList();
		//Long usuarioRolId = listaUsuario.size() > 0 ? listaUsuario.get(0).getUsuarioRolId() : null;
		return listaUsuario;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaUsuarioRolEntidadDTO> obtenerUsuarioPorEntidad(Long rolId, Long entidadId) {
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDAR_ROL_USUARIO_ENTIDAD, UserRolEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);
		//storedProcedure.setParameter(1, usuarioId);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.setParameter(2, rolId);
		storedProcedure.execute();
		List<ListaUsuarioRolEntidadDTO> listaUsuario = storedProcedure.getResultList();
		//Long usuarioRolId = listaUsuario.size() > 0 ? listaUsuario.get(0).getUsuarioRolId() : null;
		return listaUsuario;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<UserRolEntidadDTO> obtenerUsuarioPorEntidadSolicitud(Long rolId, Long entidadId) {
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDAR_ROL_ENTIDAD, UserRolEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		//storedProcedure.setParameter(1, usuarioId);
		storedProcedure.setParameter(1, rolId);
		storedProcedure.setParameter(2,  entidadId);
		storedProcedure.execute();
		List<UserRolEntidadDTO> listaUsuario = storedProcedure.getResultList();
		//Long usuarioRolId = listaUsuario.size() > 0 ? listaUsuario.get(0).getUsuarioRolId() : null;
		return listaUsuario;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public UsuarioRolEntidadPersonaDTO obtieneUsuarioRolEntidad(Long personaId, Long rolId, Long entidadId) {
		StoredProcedureQuery storedProcedure = entityManager.createStoredProcedureQuery(
				Constantes.PKG_GENERAL + "." + Constantes.SP_VALIDA_USUARIO_PERSONA_ID, UserRolEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, personaId);
		storedProcedure.setParameter(1, rolId);
		storedProcedure.setParameter(2, entidadId);
		storedProcedure.execute();
		UsuarioRolEntidadPersonaDTO usuario =  (UsuarioRolEntidadPersonaDTO) storedProcedure.getSingleResult();
		//Long usuarioRolId = listaUsuario.size() > 0 ? listaUsuario.get(0).getUsuarioRolId() : null;
		return usuario;
	}
}
