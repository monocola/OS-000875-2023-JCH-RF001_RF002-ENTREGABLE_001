package pe.gob.servir.entidad.response;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespObtieneLista3 {

	private Integer count;
	private List<Parametro> items;
	
	@Getter
	@Setter
	@ToString
	public static class Parametro {
		private Long parametroId;
		private String tipoParametro;
		private String codigoTexto;
		private Long codigoNumero;	
		private String valorTexto;
		private BigDecimal valorNumero;
		private Date valorFecha;
		private String descripcion;
		private String estadoRegistro;
		private String usuarioCreacion;
		private Date fechaCreacion;
		private String usuarioModificacion;
		private Date fechaModificacion;
		
		public Parametro() {
			super();
		}
	}
}
