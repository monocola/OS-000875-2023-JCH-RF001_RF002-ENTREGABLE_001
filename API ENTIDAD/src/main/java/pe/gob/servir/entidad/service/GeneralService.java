package pe.gob.servir.entidad.service;

import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespPaises;
import pe.gob.servir.entidad.response.RespParametro;

public interface GeneralService {
	RespBase<RespParametro> comboEstado();
	RespBase<RespParametro> comboNivel(String tipoOrganigrama);
	RespBase<RespParametro> comboNaturalezaOrgano();	
	RespBase<RespParametro> comboTipoDocumento();
	RespBase<RespPaises> comboPaises();
	
	RespBase<RespParametro> comboOrgano();
	RespBase<RespParametro> comboSexo();
	
	RespBase<RespParametro> comboParametro(Integer parametroId, String tipoParametro, String codigoTexto);
}
