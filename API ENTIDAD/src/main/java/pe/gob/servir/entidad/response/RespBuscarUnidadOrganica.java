package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.BuscarUnidadOrganica;

@Getter
@Setter
public class RespBuscarUnidadOrganica {
	
	private List<BuscarUnidadOrganica> listaComboUnidadOrganica;
}
