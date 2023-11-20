package pe.gob.servir.entidad.repository.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.ParameterMode;
import javax.persistence.PersistenceContext;
import javax.persistence.PersistenceContextType;
import javax.persistence.StoredProcedureQuery;

import org.jboss.logging.Logger;
import org.springframework.stereotype.Repository;

import pe.gob.servir.entidad.bean.ReqSpBuscarUOXServidorCivil;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.BuscarUnidadOrganica;
import pe.gob.servir.entidad.model.ComboByOrganigrama;
import pe.gob.servir.entidad.model.ComboDTO;
import pe.gob.servir.entidad.model.ComboPuesto;
import pe.gob.servir.entidad.model.ComboUnidadOrganica;
import pe.gob.servir.entidad.model.DatosUOServidorCivil;
import pe.gob.servir.entidad.model.EntidadDTO;
import pe.gob.servir.entidad.model.EntidadTipoRucDTO;
import pe.gob.servir.entidad.model.ExisteOrganigramaDTO;
import pe.gob.servir.entidad.model.GenericoDTO;
import pe.gob.servir.entidad.model.GestionOrganigramaDTO;
import pe.gob.servir.entidad.model.GestorDTO;
import pe.gob.servir.entidad.model.ListaEntidadDTO;
import pe.gob.servir.entidad.model.ListaEntidades;
import pe.gob.servir.entidad.model.ListaGestorDTO;
import pe.gob.servir.entidad.model.ListaSedeDTO;
import pe.gob.servir.entidad.model.ListarOrganigramaDTO;
import pe.gob.servir.entidad.model.ListarOrganigramaDTOTest;
import pe.gob.servir.entidad.model.ObtenerCuentaEntidadRolDTO;
import pe.gob.servir.entidad.model.ObtenerRolDTO;
import pe.gob.servir.entidad.model.OrganigramaDTO;
import pe.gob.servir.entidad.model.ResumenServidoresCivilesDTO;
import pe.gob.servir.entidad.model.ResumenServidoresCivilesGDRDTO;
import pe.gob.servir.entidad.model.ServidorCivilDTO;
import pe.gob.servir.entidad.model.ServidorRectorDTO;
import pe.gob.servir.entidad.model.SolicitudExternaDTO;
import pe.gob.servir.entidad.model.UnidadOrganicaDTO;
import pe.gob.servir.entidad.model.ValidaPersonaCuenta;
import pe.gob.servir.entidad.model.ValidaTareaCuentaEntidad;
import pe.gob.servir.entidad.model.ValidarUsuarioDTO;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.service.impl.OrganigramaServiceImpl;


@Repository
public class GestionRepositoryImpl implements GestionRepository{ 
	
	private static final Logger LOGGER = Logger.getLogger(GestionRepositoryImpl.class);

	
	@PersistenceContext(unitName = "entidadEntityManagerFactory",type = PersistenceContextType.TRANSACTION)
	EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaEntidades> listaEntidadesByRol(Map<String, Object> parametroMap) {
		List<ListaEntidades> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_CUENTAS_ASOCIADAS,ListaEntidades.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("cuentaId"));
		storedProcedure.setParameter(2, parametroMap.get("texto"));		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaEntidadDTO> listaEntidad(Long entidadId) {
		List<ListaEntidadDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ENTIDAD,ListaEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, entidadId);		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ValidarUsuarioDTO> ObtenerUsuarioExistente(String numeroDocumento) {
		List<ValidarUsuarioDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_VALIDAR_USUARIO,ValidarUsuarioDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, numeroDocumento);		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ObtenerRolDTO> listaRolesByEntidades(Map<String, Object> parametroMap) {
		List<ObtenerRolDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ROLES,ObtenerRolDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Integer.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, parametroMap.get("cuentaId"));
		storedProcedure.setParameter(2, parametroMap.get("aplicacionId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<OrganigramaDTO> buscarOrganigramaByFilter(Map<String, Object> parametroMap) {
		List<OrganigramaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ORGANIGRAMA,OrganigramaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));		
		storedProcedure.setParameter(2, parametroMap.get("tipo"));
		storedProcedure.setParameter(3, parametroMap.get("texto"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ValidaTareaCuentaEntidad> validarTareaByCuneta(Long cuentaEntidadId) {
		List<ValidaTareaCuentaEntidad> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_VALIDAR_TAREAS_CTA_ASOCIADA,ValidaTareaCuentaEntidad.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, cuentaEntidadId);		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ValidaPersonaCuenta> validarPersonaCuentaEntidad(Long personaId) {
		List<ValidaPersonaCuenta> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_VALIDA_CTA_ASOCIADA,ValidaPersonaCuenta.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, personaId);		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<OrganigramaDTO> buscarOrganigramaByPersona(Integer tipoDocumento,String numeroDocumento) {
		List<OrganigramaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_PERSONA_ORGANIGRAMA,OrganigramaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, tipoDocumento);		
		storedProcedure.setParameter(2, numeroDocumento);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<EntidadDTO> buscarEntidadByPersona(Integer tipoDocumento,String numeroDocumento) {
		List<EntidadDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_PERSONA_ENTIDAD,EntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, tipoDocumento);		
		storedProcedure.setParameter(2, numeroDocumento);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<UnidadOrganicaDTO> buscarUnidadOrganica(Long entidadId) {
		List<UnidadOrganicaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_UNIDAD_ORGANICA,UnidadOrganicaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);			
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, entidadId);			
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListarOrganigramaDTO> buscarOrganigramaPadre(Map<String, Object> parametroMap) {
		List<ListarOrganigramaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ORGANIGRAMA_PADRE,ListarOrganigramaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.setParameter(3, parametroMap.get("organigramaId"));
		storedProcedure.setParameter(4, parametroMap.get("puesto"));
		storedProcedure.setParameter(5, parametroMap.get(Constantes.ESTADO));
		storedProcedure.setParameter(6, parametroMap.get(Constantes.UNIDADID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListarOrganigramaDTO> buscarOrganigramaHijo(Map<String, Object> parametroMap) {
		List<ListarOrganigramaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ORGANIGRAMA_HIJO,ListarOrganigramaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);				
		storedProcedure.registerStoredProcedureParameter(2, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Integer.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(4, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));	
		storedProcedure.setParameter(2, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.UNIDADID));
		storedProcedure.setParameter(4, parametroMap.get("puesto"));
		storedProcedure.setParameter(5, parametroMap.get(Constantes.ESTADO));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ComboByOrganigrama> comboPersonaByOrganigrama(Map<String, Object> parametroMap) {
		List<ComboByOrganigrama> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_PERSONA_ORGANIGRAMA,ComboByOrganigrama.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);				
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));	
		storedProcedure.setParameter(2, parametroMap.get("nombreApellidos"));		
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaSedeDTO> buscarSedeByFilter(Map<String, Object> parametroMap) {
		List<ListaSedeDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCA_SEDE,ListaSedeDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);				
		storedProcedure.registerStoredProcedureParameter(2, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(3, Integer.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(4, Integer.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(5, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));	
		storedProcedure.setParameter(2, parametroMap.get("distrito"));
		storedProcedure.setParameter(3, parametroMap.get("provincia"));	
		storedProcedure.setParameter(4, parametroMap.get("departamento"));	
		storedProcedure.setParameter(5, parametroMap.get(Constantes.ESTADO));
		storedProcedure.setParameter(6, parametroMap.get("sedeId"));	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ObtenerCuentaEntidadRolDTO> listarCuentaPorEntidadPorRol(Map<String, Object> parametroMap) {
		List<ObtenerCuentaEntidadRolDTO> lista = new ArrayList<ObtenerCuentaEntidadRolDTO>();
		StoredProcedureQuery storedProcedure = entityManager
				.createStoredProcedureQuery(Constantes.PKG_GESTION + "." + Constantes.SP_BUSCAR_USUARIOS_POR_ROL_ENT, ObtenerCuentaEntidadRolDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Integer.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Integer.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("rolId"));
		storedProcedure.setParameter(3, parametroMap.get("aplicacionId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaEntidadDTO> listarEntidades() {
		List<ListaEntidadDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ENTIDADES_ACTIVAS,ListaEntidadDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ResumenServidoresCivilesDTO> resumenDeServidoresCiviles(Long entidadId) {
		List<ResumenServidoresCivilesDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES,ResumenServidoresCivilesDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ResumenServidoresCivilesGDRDTO> resumenDeServidoresCivilesGDR(Long entidadId) {
		List<ResumenServidoresCivilesGDRDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_GDR,ResumenServidoresCivilesGDRDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesTipoOrgano(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_TIPO_ORGANO,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesTipoOrganoGDR(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_SEGUN_TIPO_ORGANO_GDR,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesRegimenLaboralGDR(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_REGIMEN_LABORAL_GDR,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesCarrerasEspecialesGDR(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_CARRERAS_ESPECIALES_GDR,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesPorSegmentoGDR(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_POR_SEGMENTO_GDR,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesSindicalizadosGDR(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_SINDICALIZADOS_GDR,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);	
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GenericoDTO> resumenDeServidoresCivilesRegimenLaboral(Long entidadId) {
		List<GenericoDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_RESUMEN_SERVIDORES_CIVILES_REGIMEN_LABORAL,GenericoDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaEntidadDTO> listarEntidadesPorListId(String listId) {
		List<ListaEntidadDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ENTIDADES_POR_IDS,ListaEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, listId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ComboUnidadOrganica> buscarUnidadesOrganicas(Map<String, Object> parametroMap) {
		List<ComboUnidadOrganica> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_BUSCAR_UNIDADES_ORGANICAS_ENTIDAD,ComboUnidadOrganica.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.TIPOORGANOID));
		storedProcedure.setParameter(3, parametroMap.get("uoSupId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ComboPuesto> buscarPuestos(Long entidadId, Long organigramaId) {
		List<ComboPuesto> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_BUSCAR_PUESTOS_ENTIDAD,ComboPuesto.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.setParameter(2, organigramaId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ComboPuesto> filtrarPuestos(Map<String, Object> parametroMap) {
		List<ComboPuesto> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_FILTRAR_PUESTOS,ComboPuesto.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);			
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("descripcion"));
		storedProcedure.setParameter(3, parametroMap.get("organigramaId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ServidorCivilDTO> selectServidoresCiviles(Map<String, Object> parametroMap) {
		List<ServidorCivilDTO> lista;
		String estadoId = null;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_SEL_SERVIDORES_CIVILES,ServidorCivilDTO.class);		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(8, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(9, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(10, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.TIPOORGANOID));
		storedProcedure.setParameter(3, parametroMap.get("unidadOrganicaSuperiorId"));
		storedProcedure.setParameter(4, parametroMap.get("unidadOrganicaId"));
		storedProcedure.setParameter(5, parametroMap.get("regimenLaboralId"));
		storedProcedure.setParameter(6, parametroMap.get("tipoDocumentoId"));
		storedProcedure.setParameter(7, parametroMap.get("datosServCivil"));
		storedProcedure.setParameter(8, parametroMap.get("numeroDocumento"));
		estadoId = (parametroMap.get("estadoId") != null) ? String.valueOf(parametroMap.get("estadoId")) : null;
		storedProcedure.setParameter(9, estadoId);
		storedProcedure.execute();
	
		lista = storedProcedure.getResultList();
		System.out.print(" ltaServidorCivilFilter:"+lista.toString());
		
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<GestionOrganigramaDTO> selectGestionOrganigrama(Map<String, Object> parametroMap) {
		List<GestionOrganigramaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_SEL_GESTION_ORGANIGRAMAS,GestionOrganigramaDTO.class);
		
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class, ParameterMode.REF_CURSOR);	
		
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.TIPOORGANOID));
		storedProcedure.setParameter(3, parametroMap.get("unidadOrganicaSuperiorId"));
		storedProcedure.setParameter(4, parametroMap.get("unidadOrganicaId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<BuscarUnidadOrganica> buscarUnidadesOrganicasSuperior(Map<String, Object> parametroMap) {
		List<BuscarUnidadOrganica> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_BUSCAR_UNIDADES_ORGANICAS_SUPERIOR_ENTIDAD,BuscarUnidadOrganica.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.TIPOORGANOID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ListarOrganigramaDTOTest> buscarOrganoPadre(Map<String, Object> parametroMap) {
		List<ListarOrganigramaDTOTest> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_BUSCAR_ORGANIGRAMA_PADRE,ListarOrganigramaDTOTest.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.setParameter(3, parametroMap.get("organigramaId"));
		storedProcedure.setParameter(4, parametroMap.get("puestoId"));
		storedProcedure.setParameter(5, parametroMap.get(Constantes.ESTADO));
		storedProcedure.setParameter(6, parametroMap.get(Constantes.UNIDADID));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListarOrganigramaDTOTest> buscarOrganoHijo(Map<String, Object> parametroMap) {
		List<ListarOrganigramaDTOTest> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_BUSCAR_ORGANIGRAMA_HIJO,ListarOrganigramaDTOTest.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);				
		storedProcedure.registerStoredProcedureParameter(2, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Integer.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(4, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));	
		storedProcedure.setParameter(2, parametroMap.get(Constantes.PERSONAID));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.UNIDADID));
		storedProcedure.setParameter(4, parametroMap.get("puestoId"));
		storedProcedure.setParameter(5, parametroMap.get(Constantes.ESTADO));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ExisteOrganigramaDTO> obtenerValidaOrganigrama(Map<String, Object> parametroMap) {
		List<ExisteOrganigramaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_ORGANIGRAMA+"."+Constantes.SP_VALIDA_ORGANIGRAMA,ExisteOrganigramaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class ,   ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class ,   ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, void.class,    ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get("descripcion"));
		storedProcedure.setParameter(3, parametroMap.get("sigla"));
		storedProcedure.setParameter(4, parametroMap.get("idUO"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ListaEntidadDTO> listaEntidadFilt(Map<String, Object> parametroMap) {
		List<ListaEntidadDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ENTIDAD_FILT,ListaEntidadDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);		
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));	
		storedProcedure.setParameter(2, parametroMap.get("sectorId"));
		storedProcedure.setParameter(3, parametroMap.get("nivelGobiernoId"));
		storedProcedure.setParameter(4, parametroMap.get("tipoEntidadId"));
		storedProcedure.setParameter(5, parametroMap.get("tipoEntidadPubId"));
		storedProcedure.setParameter(6, parametroMap.get("descripcion"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ServidorRectorDTO> listarServidoresRectores(Map<String, Object> parametroMap) {

		List<ServidorRectorDTO> listaServidoresRectores;

		StoredProcedureQuery storedProcedure =
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION + "."+
				                                         Constantes.SP_BUSCAR_SERVIDORES_RECTORES, ServidorRectorDTO.class);

		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(5, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(8, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(9, void.class, ParameterMode.REF_CURSOR);

		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.TIPOORGANOID));
		storedProcedure.setParameter(3, parametroMap.get("unidadOrganicaSuperiorId"));
		storedProcedure.setParameter(4, parametroMap.get("unidadOrganicaId"));
		storedProcedure.setParameter(5, parametroMap.get("regimenLaboralId"));
		storedProcedure.setParameter(6, parametroMap.get("tipoDocumentoId"));
		storedProcedure.setParameter(7, parametroMap.get("datosServCivil"));
		storedProcedure.setParameter(8, parametroMap.get("numeroDocumento"));

		storedProcedure.execute();
		listaServidoresRectores = storedProcedure.getResultList();

		return listaServidoresRectores;
	}

	@SuppressWarnings("unchecked")
	public List<DatosUOServidorCivil> listarUOsByServCiv(ReqSpBuscarUOXServidorCivil request) {

		List<DatosUOServidorCivil> lstDatosUo;

		StoredProcedureQuery storedProcedure =
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION + "."+
						Constantes.SP_BUSCAR_UO_X_SERVIDOR_CIVIL, DatosUOServidorCivil.class);

		Map<String, Object> parametroMap = new HashMap<>();
		parametroMap.put(Constantes.ENTIDADID, request.getEntidadId());
		parametroMap.put(Constantes.TIPODOCUMENTOID, request.getTipoDocumentoId());
		parametroMap.put(Constantes.NUMERODOCUMENTO, request.getNroDocumento());

		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, Long.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(4, void.class, ParameterMode.REF_CURSOR);

		storedProcedure.setParameter(1, parametroMap.get(Constantes.ENTIDADID));
		storedProcedure.setParameter(2, parametroMap.get(Constantes.TIPODOCUMENTOID));
		storedProcedure.setParameter(3, parametroMap.get(Constantes.NUMERODOCUMENTO));

		storedProcedure.execute();

		lstDatosUo = storedProcedure.getResultList();

		return lstDatosUo;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<ListaGestorDTO> listarGestoresORH(Long entidadId) {
		List<ListaGestorDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_LISTAR_GESTORES_ORH,ListaGestorDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, entidadId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public List<EntidadTipoRucDTO> buscarTipobyRucEntidad(Integer tipoDocumento,String numeroDocumento) {
		List<EntidadTipoRucDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_TIPO_RUC,EntidadTipoRucDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, tipoDocumento);		
		storedProcedure.setParameter(2, numeroDocumento);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SolicitudExternaDTO> filtrarSolicitudExterna(Map<String, Object> parametroMap) {
		List<SolicitudExternaDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_FILTRAR_SOLICITUD_EXTERNA,SolicitudExternaDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, String.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(2, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(3, String.class , ParameterMode.IN);			
		storedProcedure.registerStoredProcedureParameter(4, Integer.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(5, Integer.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(6, String.class , ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(7, String.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(8, Long.class , ParameterMode.IN);	
		storedProcedure.registerStoredProcedureParameter(9, void.class, ParameterMode.REF_CURSOR);	
		storedProcedure.setParameter(1, parametroMap.get("rucEntidad"));
		storedProcedure.setParameter(2, parametroMap.get("razonSocial"));
		storedProcedure.setParameter(3, parametroMap.get("nombreCompleto"));
		storedProcedure.setParameter(4, parametroMap.get("estadoSolicitud"));
		storedProcedure.setParameter(5, parametroMap.get("tipoDocumento"));
		storedProcedure.setParameter(6, parametroMap.get("numeroDocumento"));
		storedProcedure.setParameter(7, parametroMap.get("anio"));
		storedProcedure.setParameter(8, parametroMap.get("solicitudExtId"));
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<GestorDTO> listaGestorORHId(Long gestorId) {
		List<GestorDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_GESTOR_ORH, GestorDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.IN);
		storedProcedure.registerStoredProcedureParameter(2, void.class, ParameterMode.REF_CURSOR);
		storedProcedure.setParameter(1, gestorId);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ComboDTO> listaAnioSolExt() {
		List<ComboDTO> lista;
		StoredProcedureQuery storedProcedure = 
				entityManager.createStoredProcedureQuery(Constantes.PKG_GESTION+"."+Constantes.SP_BUSCAR_ANIO_SOLEXT, ComboDTO.class);
		storedProcedure.registerStoredProcedureParameter(1, Long.class, ParameterMode.REF_CURSOR);
		storedProcedure.execute();
		lista = storedProcedure.getResultList();
		return lista;
		
	}

}