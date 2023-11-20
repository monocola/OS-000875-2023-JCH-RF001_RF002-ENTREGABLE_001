package pe.gob.servir.entidad.request;

import java.time.LocalDate;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ReqActualizaGestorORH {
	
	@NotNull(message = Constantes.CAMPO + " gestorId " + Constantes.ES_OBLIGATORIO)
	private Long gestorId;
	
	@NotNull(message = Constantes.CAMPO + " personaId " + Constantes.ES_OBLIGATORIO)
	private Long personaId;
	
	@NotNull(message = Constantes.CAMPO + " usuarioId " + Constantes.ES_OBLIGATORIO)
	private Long usuarioId;
	
	@NotNull(message = Constantes.CAMPO + " entidadId " + Constantes.ES_OBLIGATORIO)
	private Long entidadId;
	
	@NotNull(message = Constantes.CAMPO+ " tipoDocumento "+Constantes.ES_OBLIGATORIO)
	private Long tipoDocumento;
	
	@NotEmpty(message = Constantes.CAMPO+" numeroDocumento "+Constantes.ES_OBLIGATORIO)
	private String numeroDocumento;
	
	private LocalDate fechaNacimiento;
	
	// DATOS CORREO
	@NotNull(message = Constantes.CAMPO + " correoId " + Constantes.ES_OBLIGATORIO)
	private Long correoId;

	@Email
	@NotNull(message = Constantes.CAMPO + " correo " + Constantes.ES_OBLIGATORIO)
	private String correo;
	
	private String tipoCorreo;
	
	// DATOS TELEFONO
	private Long telefonoId;
	private String telefono;
	private String anexo;
	//private String tipoTelefono;
	
	private Long CelularId;
	private String celular;
	//private String tipoCelular;
	private Long rolId;
	
	private Long paisId;
	
	@NotNull(message = Constantes.CAMPO + " flagUPDT " + Constantes.ES_OBLIGATORIO)
	private Long flagUPDT;
}
