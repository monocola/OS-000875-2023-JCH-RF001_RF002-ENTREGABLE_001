package pe.gob.servir.entidad.repository;

import java.util.List;

import pe.gob.servir.entidad.model.CuentaEntidadRolDTO;
import pe.gob.servir.entidad.model.ListaUsuarioRolEntidadDTO;
import pe.gob.servir.entidad.model.UserRolDTO;
import pe.gob.servir.entidad.model.UserRolEntidadDTO;
import pe.gob.servir.entidad.model.UsuarioRolEntidadPersonaDTO;

public interface UsuarioRepositorySP {

	boolean existeRolUsuario(Long usuarioId, Long rolId);

	public UserRolDTO buscarRolUsuario(Long personaId, Long rolId);

	List<CuentaEntidadRolDTO> buscarRolCuentaEntidad(Long entidadId, Long rolId);

	List<UserRolEntidadDTO> buscarRolUsuarioEntidad(Long usuarioId, Long rolId, Long entidadId);
	
	List<ListaUsuarioRolEntidadDTO> obtenerUsuarioPorEntidad(Long rolId, Long entidadId);
	
	List<UserRolEntidadDTO> obtenerUsuarioPorEntidadSolicitud(Long rolId, Long entidadId);
	
	UsuarioRolEntidadPersonaDTO obtieneUsuarioRolEntidad(Long personaId, Long rolId, Long entidadId);
}
