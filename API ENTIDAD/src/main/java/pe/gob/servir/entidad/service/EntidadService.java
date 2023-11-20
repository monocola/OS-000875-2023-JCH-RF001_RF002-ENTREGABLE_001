package pe.gob.servir.entidad.service;

import java.util.Map;

import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqEntidad;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespEntidad;
import pe.gob.servir.entidad.response.RespListaEntidad;
import pe.gob.servir.entidad.response.RespListaServidoresCiviles;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGDR;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGDRGraficosDonats;
import pe.gob.servir.entidad.response.RespListaServidoresCivilesGraficosDonats;
import pe.gob.servir.entidad.security.MyJsonWebToken;

public interface EntidadService {

	RespBase<RespEntidad> actualizarEntidad(ReqBase<ReqEntidad> request, MyJsonWebToken token,Long entidadId);
	
	RespBase<RespEntidad> actualizarEntidadGme(ReqBase<ReqEntidad> request, MyJsonWebToken token,Long entidadId);
	
	RespBase<RespListaEntidad> listarEntidad(Long entidadId);

	RespBase<RespListaEntidad> listarEntidades();

	RespBase<RespListaServidoresCiviles> resumenDeServidoresCiviles(Long entidadId);
	
	RespBase<RespListaServidoresCivilesGDR> resumenDeServidoresCivilesGDR(Long entidadId);
	
	RespBase<RespListaServidoresCivilesGraficosDonats> resumenDeServidoresCivilesGraficosDonats(Long entidadId);
	
	RespBase<RespListaServidoresCivilesGDRGraficosDonats> resumenDeServidoresCivilesGDRGraficosDonats(Long entidadId);

	RespBase<RespListaEntidad> listarEntidadesPorListId(String listId);
	
	RespBase<RespListaEntidad> listarEntidadFilter(Map<String, Object> parametroMap);

	RespBase<RespListaEntidad> listarEntidadPorSigla(String sigla, boolean soloActivos);
	
}