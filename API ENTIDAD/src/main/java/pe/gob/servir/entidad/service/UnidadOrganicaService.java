package pe.gob.servir.entidad.service;

import java.io.InputStream;
import java.util.List;

import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.UnidadOrganicaExcelDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerUnidadOrganica;
import pe.gob.servir.entidad.response.RespOrganigrama;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface UnidadOrganicaService { 
	
	RespBase<RespObtenerUnidadOrganica> buscarUnidadOrganica(Long entidadId);
	
	RespBase<RespOrganigrama> guardarUnidadOrganica(ReqBase<ReqOrganigrama> request, MyJsonWebToken token,
			Long organigramaId);
	
	RespBase<Object> eliminarUnidadOrganica(MyJsonWebToken token, Long unidadOrganicaId,String estado);	
	
	List<UnidadOrganicaExcelDTO>validarCargaMasivaUnidadOrganica(InputStream uploadedInputStream); 
	
	RespBase<Object> cargaMasivaUnidadOrganica(MyJsonWebToken token,List<UnidadOrganicaExcelDTO> lista,Long entidadId);
}
