package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespApiObtenerUsuario {
	
	private Integer count;
	private List<RespApiSeguridad> items;
	
}
