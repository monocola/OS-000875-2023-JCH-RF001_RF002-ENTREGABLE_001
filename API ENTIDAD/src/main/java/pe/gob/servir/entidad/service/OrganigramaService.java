package pe.gob.servir.entidad.service;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.OrganigramaExcelDTO;
import pe.gob.servir.entidad.request.dto.OrganoExcelDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespBuscarUnidadOrganica;
import pe.gob.servir.entidad.response.RespComboPerByOrganigrama;
import pe.gob.servir.entidad.response.RespComboUnidadOrganica;
import pe.gob.servir.entidad.response.RespListaOrganigrama;
import pe.gob.servir.entidad.response.RespListaOrgano;
import pe.gob.servir.entidad.response.RespObtenerGestionOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerLtaOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerServidorCivil;
import pe.gob.servir.entidad.response.RespObtenerValidaOrganigrama;
import pe.gob.servir.entidad.response.RespOrganigrama;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface OrganigramaService {

	RespBase<RespOrganigrama> guardarOrganigrama(ReqBase<ReqOrganigrama> request, MyJsonWebToken token,
			Long organigramaId);

	RespBase<RespOrganigrama> registrarOrganigrama(ReqBase<ReqOrganigrama> request, MyJsonWebToken token,
			Long organigramaId);
	
	RespBase<RespObtenerValidaOrganigrama> verificarDuplicidadOrganigrama(Map<String, Object> parametroMap);
	
	RespBase<RespObtenerOrganigrama> buscarOrganigramaByFilter(Map<String, Object> parametroMap);
	
	RespBase<RespObtenerOrganigrama> buscarOrganigramas();

	RespBase<Object> eliminarOrganigrama(MyJsonWebToken token, Long organigramaId,String estado);
	
	RespBase<Object> eliminarGestionOrganigrama(MyJsonWebToken token, Long organigramaId,String estado);
	
	RespBase<RespObtenerLtaOrganigrama> buscarOrganigramaByTipo(Long tipo,Long entidadId);
	
	RespBase<RespComboUnidadOrganica> buscarUnidadesOrganicasPorEntidad(Map<String, Object> parametroMap);
	
	RespBase<RespBuscarUnidadOrganica> buscarUnidadOrganicaSuperior(Map<String, Object> parametroMap);
	
	RespBase<RespListaOrgano> buscarOrganigramaV2(Map<String, Object> parametroMap);
	
	RespBase<RespListaOrganigrama> buscarOrganigramas(Map<String, Object> parametroMap);
	
	RespBase<RespComboPerByOrganigrama> comboPersonaByOrganigrama(Map<String, Object> parametroMap);
	
	List<OrganoExcelDTO> validarCargaMasivaOrganigrama(InputStream uploadedInputStream); 
	
	List<OrganigramaExcelDTO> validarCargaMasivaOrgExcel(InputStream uploadedInputStream);
	
	RespBase<Object> cargaMasivaOrganigrama(MyJsonWebToken token, List<OrganoExcelDTO> lista, Long entidadId);
	
	RespBase<Object> cargaMasivaOrganigramaSGM(MyJsonWebToken token, List<OrganigramaExcelDTO> lista, Long entidadId);
		
	RespBase<RespObtenerServidorCivil> selectServidoresCiviles(Map<String, Object> parametroMap);

	RespBase<RespObtenerGestionOrganigrama> selectGestionOrganigrama(Map<String, Object> parametroMap);

	RespBase<RespOrganigrama> guardarOrganigramaSGM(ReqBase<ReqOrganigrama> request, MyJsonWebToken token);

	List<OrganigramaExcelDTO> obetenerCodigoCombo(byte[] combos, List<OrganigramaExcelDTO> lista);

}
