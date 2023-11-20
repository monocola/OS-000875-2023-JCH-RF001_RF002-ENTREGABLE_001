package pe.gob.servir.entidad.response;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RespObtienePerfilUsuario implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Long personaId;
	private Long entidadId;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String nombres;
	private String tipoDocumento;
	private String numeroDocumento;
	private String telefono;
	private String genero;
	private String fechaNacimiento;
	private String correoInstitucional;
	private String correoAlternativo;
	private Long regimenLaboralId;
	private String regimenLaboral;
	private String sindicato;
	private String urlFoto;

}