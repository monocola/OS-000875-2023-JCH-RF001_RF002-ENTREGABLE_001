package pe.gob.servir.entidad.response;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespObtenerPais {
	
	private Integer count;
	private List<Pais> items;
	
	@Getter
	@Setter
	@ToString
	public static class Pais {
		
		private Long paisId;
		private String nombrePais;
		private String nacionalidad;
		private String codSunat;
		private String estadoRegistro;
		
		public Pais() {
			super();
		}
	}
}
