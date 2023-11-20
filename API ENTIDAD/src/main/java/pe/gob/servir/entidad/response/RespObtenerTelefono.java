package pe.gob.servir.entidad.response;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespObtenerTelefono {
	
	private Integer count;
	private List<Telefono> items;
	
	@Getter
	@Setter
	@ToString
	public static class Telefono {
		
		private Long telefonoId;
		private Long personaId;
		private String tipoTelefono;
		private String codigoArea;
		private String numeroTelefono;
		private String numeroAnexo;
		private String estadoRegistro;
        private String usuarioCreacion;
        private String fechaCreacion;
        private String usuarioModificacion;
        private String fechaModificacion;
		
		public Telefono() {
			super();
		}
	}
}
