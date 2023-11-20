package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter   

public class RespListaEntidades {
	private List<Entidades> listaEntidades;
	
	@Getter
	@Setter	
	public static class Entidades {	
		private Long nroCuentas;
		private Long cuentaId;
		private Integer usuarioId;
		private Integer personaId;
		private String nombres;
		private String apellidoMaterno;			
		private String apellidoPaterno;
		private String descripcionPuesto;
		private Integer documentoId;
		private Integer tipoDocumento;
		private String descTipoDocumento;
		private String nroDocumento;
		private String estadoId;
		private String flagCuentaEditable;
		private String descripcionEstado;
		private Integer paisId;
		private String fechaAlta;
		private Integer correoId;
		private String nombrePais;
		private String correo;
		private String fechaBaja;
		private Integer telefonoId;
		private String numeroTelefono;
		private String anexo;
		private List<Roles> listaRoles;
		
		public Entidades() {
			super();
		}
	}
		
		@Getter
		@Setter
		
		public static class Roles {	
			
			private Long nroRoles;
			private Long usuarioRolId;
			private Long rolId;
			private String nombreRol;
			private String estadoId;
			private String descripcionEstado;
			private Long cuentaId;
			private String fechaAltaRol;
			private String fechaBajaRol;
			
			public Roles() {
				super();
			}
	}

}
