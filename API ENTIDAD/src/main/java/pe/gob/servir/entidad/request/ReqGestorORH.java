package pe.gob.servir.entidad.request;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ReqGestorORH {
	@NotNull(message = Constantes.CAMPO+ " entidadId "+Constantes.ES_OBLIGATORIO)
	private Long entidadId;
	@NotNull(message = Constantes.CAMPO+ " tipoDocumento "+Constantes.ES_OBLIGATORIO)
	private Long tipoDocumento;
	@NotEmpty(message = Constantes.CAMPO+" documento "+Constantes.ES_OBLIGATORIO)
	private String numeroDocumento;
	@NotEmpty(message = Constantes.CAMPO+" documento "+Constantes.ES_OBLIGATORIO)
	private String nombres;
	@NotEmpty(message = Constantes.CAMPO+" apellidoParteno "+Constantes.ES_OBLIGATORIO)
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String fechaNacimiento;
	@Email
	@NotEmpty(message = Constantes.CAMPO+" correo "+Constantes.ES_OBLIGATORIO)	
	private String correo;
	private String telefono;
	@NotNull(message = Constantes.CAMPO+ " rolId "+Constantes.ES_OBLIGATORIO)
	private Long rolId;
	
	private Long paisId;
	

}
