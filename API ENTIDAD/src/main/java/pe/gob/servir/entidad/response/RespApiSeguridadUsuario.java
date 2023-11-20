package pe.gob.servir.entidad.response;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespApiSeguridadUsuario {
	
	private Long usuarioId;
	private String usuario;
	private String correoElectronico;
	private String cambiaPassword;
	private String ldap;
	private Long personaId;
	private String estadoUsuario;
	private String urlAvatar;
	private String estadoRegistro;
	private String usuarioCreacion;
	private String usuarioModificacion;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String nombres;
	private String passwordTemporal;

    private String fechaFinBloqueo;
    
    private String fechaInicioVigencia;
    
	private String fechaFinVigencia;
	// private String password;
}
