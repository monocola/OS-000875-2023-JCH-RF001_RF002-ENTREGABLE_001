package pe.gob.servir.entidad.response;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RespPersonaServidorCivil implements Serializable{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Long detalleuoId;
	private Long personaId;
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
	private String regimenLaboral;
	private String sindicato;
	private Long uoId;
	private String urlFoto;
	private Long regimenId;
}
