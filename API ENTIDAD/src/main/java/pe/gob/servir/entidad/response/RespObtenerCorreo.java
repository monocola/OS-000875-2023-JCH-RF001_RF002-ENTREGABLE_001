package pe.gob.servir.entidad.response;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespObtenerCorreo {
	
	private Integer count;
	private List<Correo> items;
	private String codigoConfirmacion;
	@Getter
	@Setter
	@ToString
	public static class Correo {
		
		private Long correoId;
		private Long personaId;
		private String tipoCorreo;
		private String correo;
		private String estadoRegistro;
        private String usuarioCreacion;
        private String fechaCreacion;
        private String usuarioModificacion;
        private String fechaModificacion;
		
		public Correo() {
			super();
		}
	}
}
