package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.UnidadOrganicaDTO;

@Getter
@Setter
public class RespObtenerUnidadOrganica {
	private List<UnidadOrganicaDTO> listaUnidadOrganica;

}
