package pe.gob.servir.entidad.repository;

import java.util.List;

import pe.gob.servir.entidad.model.CuentaEntidadDTO;
import pe.gob.servir.entidad.model.PaisesDTO;
import pe.gob.servir.entidad.model.Parametro;

public interface GeneralRepository {

	public List<Parametro> buscarListaParametro(Integer parametroId, String tipoParametro, String codigoTexto);
	
	public Parametro buscarParametro(Integer parametroId, String tipoParametro, String codigoTexto);

	List<CuentaEntidadDTO> buscarCuentaEntidad(Long entidadId); 
	
	public List<PaisesDTO> buscarPaises();
	
}
