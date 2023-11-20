package pe.gob.servir.entidad.repository;

import java.util.List;
import java.util.Map;

import pe.gob.servir.entidad.bean.ReqSpBuscarUOXServidorCivil;
import pe.gob.servir.entidad.model.*;

public interface GestionRepository {
	
	List<ListaEntidades> listaEntidadesByRol(Map<String, Object> parametroMap);

	List<ObtenerRolDTO> listaRolesByEntidades(Map<String, Object> parametroMap);

	List<ListaEntidadDTO> listaEntidad(Long entidadId);

	List<ValidarUsuarioDTO> ObtenerUsuarioExistente(String numeroDocumento);

	List<OrganigramaDTO> buscarOrganigramaByFilter(Map<String, Object> parametroMap);
	
	List<ValidaTareaCuentaEntidad> validarTareaByCuneta(Long cuentaEntidadId);
	
	List<ValidaPersonaCuenta> validarPersonaCuentaEntidad(Long personaId);

	List<OrganigramaDTO> buscarOrganigramaByPersona(Integer tipoDocumento, String numeroDocumento);

	List<EntidadDTO> buscarEntidadByPersona(Integer tipoDocumento, String numeroDocumento);
	
	List<UnidadOrganicaDTO> buscarUnidadOrganica(Long entidadId);
	
	List<ListarOrganigramaDTO> buscarOrganigramaPadre(Map<String, Object> parametroMap);
	
	List<ListarOrganigramaDTO> buscarOrganigramaHijo(Map<String, Object> parametroMap);
	
	List<ComboByOrganigrama> comboPersonaByOrganigrama(Map<String, Object> parametroMap);
	
	List<ListaSedeDTO> buscarSedeByFilter(Map<String, Object> parametroMap);
	
	List<ObtenerCuentaEntidadRolDTO> listarCuentaPorEntidadPorRol(Map<String, Object> parametroMap);

	List<ListaEntidadDTO> listarEntidades();

	List<ResumenServidoresCivilesDTO> resumenDeServidoresCiviles(Long entidadId);
	
	List<ResumenServidoresCivilesGDRDTO> resumenDeServidoresCivilesGDR(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesTipoOrgano(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesTipoOrganoGDR(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesRegimenLaboral(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesRegimenLaboralGDR(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesCarrerasEspecialesGDR(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesPorSegmentoGDR(Long entidadId);
	
	List<GenericoDTO> resumenDeServidoresCivilesSindicalizadosGDR(Long entidadId);
	
	List<ListaEntidadDTO> listarEntidadesPorListId(String listId);
	
	List<ComboUnidadOrganica> buscarUnidadesOrganicas(Map<String, Object> parametroMap);
	
	List<BuscarUnidadOrganica> buscarUnidadesOrganicasSuperior(Map<String, Object> parametroMap);
	
	List<ComboPuesto> buscarPuestos(Long entidadId, Long organigramaId);
	
	List<ComboPuesto> filtrarPuestos(Map<String, Object> parametroMap);
	
	List<ListarOrganigramaDTOTest> buscarOrganoPadre(Map<String, Object> parametroMap);
	
	List<ListarOrganigramaDTOTest> buscarOrganoHijo(Map<String, Object> parametroMap);
	
	List<ServidorCivilDTO> selectServidoresCiviles(Map<String, Object> parametroMap);
	
	List<GestionOrganigramaDTO> selectGestionOrganigrama(Map<String, Object> parametroMap);

	List<ExisteOrganigramaDTO> obtenerValidaOrganigrama(Map<String, Object> parametroMap);
	
	List<ListaEntidadDTO> listaEntidadFilt(Map<String, Object> parametroMap);

	List<ServidorRectorDTO> listarServidoresRectores (Map<String, Object> parametroMap);

	List<DatosUOServidorCivil> listarUOsByServCiv (ReqSpBuscarUOXServidorCivil request);
	
	List<ListaGestorDTO> listarGestoresORH(Long entidadId);
	
	List<EntidadTipoRucDTO> buscarTipobyRucEntidad(Integer tipoDocumento, String numeroDocumento);

	List<SolicitudExternaDTO> filtrarSolicitudExterna(Map<String, Object> parametroMap);
	
	List<GestorDTO> listaGestorORHId(Long gestorId);
	
	List<ComboDTO> listaAnioSolExt();
}
