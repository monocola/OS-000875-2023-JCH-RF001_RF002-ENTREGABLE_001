package pe.gob.servir.entidad.response;


import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.model.ListaBannersDTO;

@Getter
@Setter
@ToString
public class RespListaBanners {
	private List<ListaBannersDTO> banners;
}
