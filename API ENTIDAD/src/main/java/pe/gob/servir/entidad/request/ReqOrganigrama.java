package pe.gob.servir.entidad.request;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ReqOrganigrama {

	@NotNull(message = Constantes.CAMPO + " entidadId " + Constantes.ES_OBLIGATORIO)
	private Long entidadId;

	private Long areaId;

	private Long sedeId;
	
	private String estadoRegistro;

	private Long padreOrganigramaId;

	@Size(max = 200, message = Constantes.CAMPO + " puesto " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String puesto;

	@NotNull(message = Constantes.CAMPO + " orden " + Constantes.ES_OBLIGATORIO)
	private Integer orden;

	private Long nivel;

	@NotNull(message = Constantes.CAMPO + " tipoOrganoUoId " + Constantes.ES_OBLIGATORIO)
	private Long tipoOrganoUoId;

	private Long naturalezaOrgano;

	@Size(max = 200, message = Constantes.CAMPO + " nombres " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String nombres;
	@Size(max = 200, message = Constantes.CAMPO + " apellidoPaterno " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String apellidoPaterno;
	@Size(max = 200, message = Constantes.CAMPO + " apellidoMaterno " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String apellidoMaterno;

	@Size(max = 30, message = Constantes.CAMPO + " numeroDocumento " + Constantes.ES_INVALIDO + ", máximo 30 "
			+ Constantes.CARACTERES)
	private String numeroDocumento;

	private Long nivelGobiernoId;

	@NotNull(message = Constantes.CAMPO + " descripcion " + Constantes.ES_OBLIGATORIO)
	@Size(max = 200, message = Constantes.CAMPO + " nombres " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String descripcion;

	@Size(max = 50, message = Constantes.CAMPO + " descripcionCorta " + Constantes.ES_INVALIDO + ", máximo 50 "
			+ Constantes.CARACTERES)
	private String descripcionCorta;

	private Integer tipoDocumento;

	@Size(max = 10, message = Constantes.CAMPO + " sigla " + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String sigla;

	private Long telefonoId;
	@Size(max = 25, message = Constantes.CAMPO + " telefono " + Constantes.ES_INVALIDO + ", máximo 25 "
			+ Constantes.CARACTERES)
	private String telefono;

	private Long correoId;
	@Size(max = 150, message = Constantes.CAMPO + " correoPrincipal " + Constantes.ES_INVALIDO + ", máximo 150 "
			+ Constantes.CARACTERES)
	@Email(message = Constantes.CAMPO + " correo " + Constantes.ES_INVALIDO + "")
	private String correo;
	
	private Long paisId; 
}
