package pe.gob.servir.entidad.request;

import javax.validation.Valid;
import javax.validation.constraints.Email;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.request.dto.LogoDTO;

@Getter
@Setter
public class ReqEditaServidorCivil {

	private Long detuoId;
	
	private Long entidadId;
	
	private Integer personaId;

	@Size(max = 25, message = Constantes.CAMPO + " telefono " + Constantes.ES_INVALIDO + ", máximo 25 "
			+ Constantes.CARACTERES)
	private String telefono;

	@Size(max = 150, message = Constantes.CAMPO + " correoPrincipal " + Constantes.ES_INVALIDO + ", máximo 150 "
			+ Constantes.CARACTERES)
	@Email(message = Constantes.CAMPO + " correo " + Constantes.ES_INVALIDO + "")
	private String correoInstitucional;
	
	@Size(max = 150, message = Constantes.CAMPO + " correoAlterno " + Constantes.ES_INVALIDO + ", máximo 150 "
			+ Constantes.CARACTERES)
	@Email(message = Constantes.CAMPO + " correo " + Constantes.ES_INVALIDO + "")
	private String correoAlterno;
	
	@Size(max = 25, message = Constantes.CAMPO + " sindicatoFlag " + Constantes.ES_INVALIDO + ", máximo 1 "
			+ Constantes.CARACTERES)
	private String sindicatoFlag;
	
	@Valid
	private LogoDTO foto;
	
	private Long regimenLaboralId;
	
}
