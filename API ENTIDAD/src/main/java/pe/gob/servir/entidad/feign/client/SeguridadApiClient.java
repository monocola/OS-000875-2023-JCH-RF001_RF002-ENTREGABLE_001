package pe.gob.servir.entidad.feign.client;

import java.util.List;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import pe.gob.servir.entidad.api.dto.ApiActualizarEstadoRolUsuario;
import pe.gob.servir.entidad.api.dto.ApiActualizarRolUsuario;
import pe.gob.servir.entidad.api.dto.ApiSeguridadRequestDTO;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.response.RespApiObtenerUsuario;
import pe.gob.servir.entidad.response.RespApiSeguridad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespRolItem;

@FeignClient(name = "seguridadApi", url = "${jboss.private.base.url.seguridad}")
public interface SeguridadApiClient {
	
	// @formatter:off
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_CREAR_USUARIO, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiSeguridad> registrarUsuario(RespBase<ApiSeguridadRequestDTO> request);
	
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_USUARIO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiSeguridad> obtenerUsuariobyID(@PathVariable("usuarioId") Long usuarioId);
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_ASIGNAR_ROLES, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<Object> asignarRolUsuario(RespBase<AsignaRolRequestDTO> request);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_BUSCAR_ROLES_USUARIO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<List<RespRolItem>> buscarRolesUsuario(@PathVariable("usuarioId") Long usuarioId);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_BUSCAR_USUARIO_FILTRO, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiObtenerUsuario> buscarUsuariosByFiltro(@RequestParam(value = "usuario") String usuario,
											@RequestParam(value = "correoElectronico") String correoElectronico,
											@RequestParam(value = "personaId") Integer personaId,
											@RequestParam(value = "estadoRegistro") String estadoRegistro);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_BUSCAR_USUARIO_FILTRO_EQ, 
			produces = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiObtenerUsuario> buscarUsuariosByFiltroExacto(@RequestParam(value = "usuario") String usuario,
											@RequestParam(value = "correoElectronico") String correoElectronico,
											@RequestParam(value = "personaId") Integer personaId,
											@RequestParam(value = "estadoRegistro") String estadoRegistro);
	
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_USUARIO_ENTIDAD, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiSeguridad> registrarUsuarioEntidad(@PathVariable("entidadId") Long entidadId,
												RespBase<ApiSeguridadRequestDTO> request);
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_ACTUALIZAR_ESTADO_ROL, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<Object> actualizarEstadoRol(@PathVariable("usuarioRolId") Long usuarioRolId,
												RespBase<ApiActualizarRolUsuario> request);
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_ASIGNAR_USUARIO_ENTIDAD, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<Object> asignarUsuarioEntidad(@PathVariable("entidadId") Long entidadId,
													 @PathVariable("usuarioId") Long usuarioId,
													 @PathVariable("estadoRegistro") String estadoRegistro);
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_ASIGNA_GRUPO_ENTIDAD, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<Object> asignarGrupoEntidad(@PathVariable("entidadId") Long entidadId,
													 @PathVariable("grupoId") Long grupoId,
													 @PathVariable("usuarioId") Long usuarioId);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_USUARIO_ENTIDAD, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiObtenerUsuario> buscarUsuarioEntidad(@PathVariable("entidadId") Long entidadId);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_OBTENER_ROLES_APLICACION,
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	RespBase<Object> obtenerRolesAplicacion(@PathVariable("aplicacionId") Long aplicacionId);
	
	@RequestMapping(method = RequestMethod.PUT, value = Constantes.ENDPOINT_ACTUALIZAR_ESTADO_ROL_USUARIO_SOLICITUD, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<Object> actualizarEstadoRolUsuario(@PathVariable("usuarioRolId") Long usuarioRolId,
												RespBase<ApiActualizarEstadoRolUsuario> request);
	
	@RequestMapping(method = RequestMethod.POST, value = Constantes.ENDPOINT_USUARIO_ENTIDAD_SOLI_EXTERNA, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiSeguridad> registrarUsuarioEntidadSolExt(@PathVariable("entidadId") Long entidadId,
												RespBase<ApiSeguridadRequestDTO> request, @PathVariable("nameUsuario") String nameUsuario);
	
	@RequestMapping(method = RequestMethod.GET, value = Constantes.ENDPOINT_USUARIO_ENTIDAD_SOLICITUDEXT, 
			produces = {MediaType.APPLICATION_JSON_VALUE },
			consumes  = {MediaType.APPLICATION_JSON_VALUE })
	RespBase<RespApiObtenerUsuario> buscarUsuarioEntidadSolicitudExt(@PathVariable("entidadId") Long entidadId);
	// @formatter:on
}
