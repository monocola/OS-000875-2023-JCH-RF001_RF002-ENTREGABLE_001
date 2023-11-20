package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespObtieneLista {

	private Integer count;
	private List<Parametro> items;
	
	@Getter
	@Setter
	@ToString
	public static class Parametro {
		private Long parametroId;
		private String tipoParametro;
		private String codigoTexto;
		private Integer codigoNumero;	
		private String valorTexto;
		private Integer valorNumero;
		private String descripcion;
		private String estadoRegistro;
		
		public Parametro() {
			super();
		}
	}
}
