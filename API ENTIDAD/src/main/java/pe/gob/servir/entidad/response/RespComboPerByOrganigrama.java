package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.ComboByOrganigrama;

@Getter
@Setter
public class RespComboPerByOrganigrama {
	private List<ComboByOrganigrama> comboPersonas;

}
