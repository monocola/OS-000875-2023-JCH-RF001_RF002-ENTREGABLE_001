package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ComboUnidadOrganica;

@Getter
@Setter
public class RespComboUnidadOrganica {
	
	private List<ComboUnidadOrganica> listaComboUnidadOrganica;
}
