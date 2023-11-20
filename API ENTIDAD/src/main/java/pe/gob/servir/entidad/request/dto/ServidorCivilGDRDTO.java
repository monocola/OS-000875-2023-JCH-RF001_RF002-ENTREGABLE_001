package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;
import java.time.LocalDate;

import javax.validation.Valid;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ServidorCivilGDRDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Valid
	@NotNull(message = "Campo nombres es obligatorio")
	@Size(max = 200, message = "Campo nombres es inválido, máximo 200 caracteres")
	private String nombres;

	@Valid
	@NotNull(message = "Campo apellido paterno es obligatorio")
	@Size(max = 200, message = "Campo nombres es inválido, máximo 200 caracteres")
	private String apellidoPaterno;

	@Valid
	@Size(max = 200, message = "Campo apellido materno es inválido, máximo 200 caracteres")
	private String apellidoMaterno;

	@Valid
	@Pattern(regexp = "1|2", message = "Campo sexo es obligatorio, valores permitidos 1(Masculino), 2(Femenino)")
	private String sexo;

	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private LocalDate fechaNacimiento;

	@Valid
	@Size(max = 150, message = "Campo correo electronico es obligatorio, máximo 150 caracteres")
	@Email(message = "Campo correo electronico es inválido")
	private String correoElectronico;

	@Valid
	@NotNull(message = "Campo tipodocumento es obligatorio")
	private Integer tipoDocumento;

	@Valid
	@NotNull(message = "Campo numeroDocumento es obligatorio")
	@Size(max = 30, message = "Campo numeroDocumento es inválido, máximo 30 caracteres")
	private String numeroDocumento;

	private String sindicatoId;

	@Valid
	@NotNull(message = "Campo órgano/uo/sub UO es obligatorio")
	private Long organoId;

	@Valid
	@NotNull(message = "Campo tipo de Asiganación es obligatorio")
	private Integer tipoAsignacion;

	private Long idDetUOPersonaAsingada;
	
	private Long puestoId;

	@Valid
	@NotNull(message = "Campo puesto descripcion es obligatorio")
	private String puestoDescripcion;

	@JsonFormat(pattern = Constantes.FORMATO_FECHA_DD_MM_YYYY, timezone = Constantes.FORMATO_TIMEZONE)
	private LocalDate fechaInicio;

	private String responsable;

	@Valid
	@NotNull(message = "Campo regimen laboral es obligatorio")
	private Long regimenLaboralId;

}