package pe.gob.servir.entidad.request.dto;

import static pe.gob.servir.entidad.util.ParametrosUtil.isNumeric;
import static pe.gob.servir.entidad.util.ParametrosUtil.validarCantidadCarecteres;
import static pe.gob.servir.entidad.util.ParametrosUtil.validarCorreo;
import static pe.gob.servir.entidad.util.ParametrosUtil.validarMaxNroCaracteres;

import java.text.ParseException;
import java.text.SimpleDateFormat;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import org.apache.commons.lang3.StringUtils;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.common.Constantes;

@JGlobalMap
@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
public class ServidorCivilExcelDTO {

	@NotNull(message = Constantes.CAMPO + "entidadId " + Constantes.ES_OBLIGATORIO)
	private String entidadId;

	@NotNull(message = Constantes.CAMPO + "tipoDocumento " + Constantes.ES_OBLIGATORIO)
	@ToString.Include
	private String tipoDocumento;

	@Size(max = 30, message = Constantes.CAMPO + " numeroDocumento " + Constantes.ES_INVALIDO + ", máximo 30 "
			+ Constantes.CARACTERES)
	@ToString.Include
	private String numeroDocumento;

	@Size(max = 200, message = Constantes.CAMPO + " apellidoPaterno " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	@ToString.Include
	private String apellidoPaterno;

	@Size(max = 200, message = Constantes.CAMPO + " apellidoMaterno " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	@ToString.Include
	private String apellidoMaterno;

	@Size(max = 200, message = Constantes.CAMPO + " nombres " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	@ToString.Include
	private String nombres;

	@NotNull(message = Constantes.CAMPO + "sexo " + Constantes.ES_OBLIGATORIO)
	private String sexo;

	@NotNull(message = Constantes.CAMPO + "fechaNacimiento " + Constantes.ES_OBLIGATORIO)
	private String fechaNacimiento;

	@NotNull(message = Constantes.CAMPO + "regimenLaboral " + Constantes.ES_OBLIGATORIO)
	private String regimenLaboral;

	@Size(max = 150, message = Constantes.CAMPO + " correoLaboral " + Constantes.ES_INVALIDO + ", máximo 150 "
			+ Constantes.CARACTERES)
	@Email(message = Constantes.CAMPO + " correo " + Constantes.ES_INVALIDO + "")
	@ToString.Include
	private String correoLaboral;

	private String sindicato;

	@Size(max = 10, message = Constantes.CAMPO + " sigla " + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String sigla;

	@Size(max = 500, message = Constantes.CAMPO + " puesto " + Constantes.ES_INVALIDO + ", máximo 500 "
			+ Constantes.CARACTERES)
	private String puesto;

	@NotNull(message = Constantes.CAMPO + "fechaInicio " + Constantes.ES_OBLIGATORIO)
	private String fechaInicio;

	@NotNull(message = Constantes.CAMPO + "responsable " + Constantes.ES_OBLIGATORIO)
	private String responsable;

	/* ID DE LOS SELECTS */
	@NotNull(message = Constantes.CAMPO + "documentoId " + Constantes.ES_OBLIGATORIO)
	private String documentoId;

	@NotNull(message = Constantes.CAMPO + "sexoId " + Constantes.ES_OBLIGATORIO)
	private String sexoId;

	@NotNull(message = Constantes.CAMPO + "regimenId " + Constantes.ES_OBLIGATORIO)
	private String regimenId;

	@NotNull(message = Constantes.CAMPO + "sindicatoId " + Constantes.ES_OBLIGATORIO)
	private String sindicatoId;

	@NotNull(message = Constantes.CAMPO + "siglaId " + Constantes.ES_OBLIGATORIO)
	private String siglaId;

	@NotNull(message = Constantes.CAMPO + "puestoId " + Constantes.ES_OBLIGATORIO)
	private String puestoId;

	private String observacion;

	@ToString.Include
	private String filaObservacion;
	private boolean flagRegistrar;
	private String observacionResultado;

	public String getAttribute(String cabecera) {
		String attribute = "";
		switch (cabecera.toUpperCase()) {
		case "TIPO DOCUMENTO":
			attribute = "tipoDocumento";
			break;
		case "NRO DOCUMENTO":
			attribute = "numeroDocumento";
			break;
		case "APELLIDO PATERNO":
			attribute = "apellidoPaterno";
			break;
		case "APELLIDO MATERNO":
			attribute = "apellidoMaterno";
			break;
		case "NOMBRES":
			attribute = "nombres";
			break;
		case "SEXO":
			attribute = "sexo";
			break;
		case "FECHA NACIMIENTO":
			attribute = "fechaNacimiento";
			break;
		case "REGIMEN LABORAL":
			attribute = "regimenLaboral";
			break;
		case "CORREO INSTITUCIONAL":
			attribute = "correoLaboral";
			break;
		case "SINDICATO (OPCIONAL)":
			attribute = "sindicato";
			break;
		case "SIGLA ORGANO, UO, SUB UO":
			attribute = "sigla";
			break;
		case "PUESTO":
			attribute = "puesto";
			break;
		case "FECHA INICIO PUESTO":
			attribute = "fechaInicio";
			break;
		case "RESPONSABLE DE ORGANO / UO":
			attribute = "responsable";
			break;
		case "ID_DOCUMENTO":
			attribute = "documentoId";
			break;
		case "ID_SEXO":
			attribute = "sexoId";
			break;
		case "ID_REGIMEN":
			attribute = "regimenId";
			break;
		case "ID_SINDICATO":
			attribute = "sindicatoId";
			break;
		case "ID_SIGLA":
			attribute = "siglaId";
			break;
		case "ID_PUESTO":
			attribute = "puestoId";
			break;
		default:
			break;
		}

		return attribute;
	}

	public String getObservacion() {// NOSONAR
		observacion = "";
		flagRegistrar = true;
		correoLaboral = correoLaboral != null ? correoLaboral.trim().toLowerCase() : correoLaboral;

		tipoDocumento = StringUtils.defaultString(tipoDocumento);
		sexo = StringUtils.defaultString(sexo);
		regimenLaboral = StringUtils.defaultString(regimenLaboral);
		sigla = StringUtils.defaultString(sigla);
		responsable = StringUtils.defaultString(responsable);
		sindicato = StringUtils.defaultString(sindicato);

		numeroDocumento = StringUtils.defaultString(numeroDocumento);

		try {
			if (tipoDocumento.equals(Constantes.VACIO) && sexo.equals(Constantes.VACIO)
					&& regimenLaboral.equals(Constantes.VACIO) && sigla.equals(Constantes.VACIO)
					&& responsable.equals(Constantes.VACIO) && StringUtils.isEmpty(numeroDocumento)
					&& StringUtils.isEmpty(apellidoPaterno) && StringUtils.isEmpty(apellidoMaterno)
					&& StringUtils.isEmpty(nombres) && StringUtils.isEmpty(fechaNacimiento)
					&& StringUtils.isEmpty(correoLaboral) && StringUtils.isEmpty(puesto)
					&& StringUtils.isEmpty(fechaInicio) && StringUtils.isEmpty(responsable)) {
				flagRegistrar = false;
				return observacion;
			}

			if (Constantes.CERO.equals(Long.valueOf(documentoId))) {
				this.observacion = this.observacion + "Debe seleccionar un tipo de documento valido,";
			}
			if (tipoDocumento.equalsIgnoreCase(Constantes.VACIO) || !validarCantidadCarecteres(tipoDocumento, 2)) {
				this.observacion = this.observacion + "Seleccione Tipo de Documento,";
			}
			if (Constantes.CERO.equals(Long.valueOf(sexoId))) {
				this.observacion = this.observacion + "Debe seleccionar un sexo valido,";
			}
			if (sexo.equalsIgnoreCase(Constantes.VACIO) || !validarCantidadCarecteres(sexo, 1)) {
				this.observacion = this.observacion + "Seleccione Sexo,";
			}
			if (fechaNacimiento == null || fechaNacimiento.trim().equalsIgnoreCase(Constantes.VACIO)) {
				this.observacion = this.observacion + "Ingrese Fecha de Nacimiento,";
			} else {
				try {
			        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
			        sdf.setLenient(false);
			        sdf.parse(fechaNacimiento);
				} catch (ParseException e) {
					this.observacion = this.observacion + "Ingrese Fecha de Nacimiento valida,";
				}
			}
			if (Constantes.CERO.equals(Long.valueOf(regimenId))) {
				this.observacion = this.observacion + "Debe seleccionar un Regimen Laboral valido,";
			}
			if (regimenLaboral.equalsIgnoreCase(Constantes.VACIO) || !validarCantidadCarecteres(regimenLaboral, 3)) {
				this.observacion = this.observacion + "Seleccione Regimen Laboral,";
			}
			if (Constantes.CERO.equals(Long.valueOf(siglaId))) {
				this.observacion = this.observacion + "Debe seleccionar una Sigla UO valida,";
			}
			if (sigla.equalsIgnoreCase(Constantes.VACIO) || !validarCantidadCarecteres(sigla, 1)) {
				this.observacion = this.observacion + "Seleccione Sigla,";
			}
			if (Constantes.CERO.equals(Long.valueOf(puestoId))) {
				this.observacion = this.observacion + "Debe seleccionar un Puesto valido,";
			}
			if (fechaInicio == null || fechaInicio.trim().equalsIgnoreCase(Constantes.VACIO)) {
				this.observacion = this.observacion + "Ingrese Fecha de Inicio Laboral,";
			} else {
				try {
			        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
			        sdf.setLenient(false);
			        sdf.parse(fechaInicio);
				} catch (ParseException e) {
					this.observacion = this.observacion + "Ingrese Fecha de Inicio Laboral valida,";
				}
			}
			if (responsable.equalsIgnoreCase(Constantes.VACIO) || !validarCantidadCarecteres(responsable, 1)) {
				this.observacion = this.observacion + "Seleccione si es Responsable,";
			}
			if (!validarCantidadCarecteres(apellidoPaterno, 2) || !validarMaxNroCaracteres(apellidoPaterno, 200)) {
				this.observacion = this.observacion + "Ingrese Apellido Paterno,";
			}
			if (!validarCantidadCarecteres(nombres, 2) || !validarMaxNroCaracteres(nombres, 200)) {
				this.observacion = this.observacion + "Ingrese Nombre,";
			}

			if (!validarCantidadCarecteres(apellidoMaterno, 2) || !validarMaxNroCaracteres(apellidoMaterno, 200)) {
				this.observacion = this.observacion + "Ingrese Apellido Materno,";
			}

			if (tipoDocumento.contains("DNI")) {
				if (!validarCantidadCarecteres(numeroDocumento, 8) || !isNumeric(numeroDocumento)) {
					this.observacion = this.observacion + "Ingrese Nro Documento correcto,";
				}
			}
			if (tipoDocumento.contains("CE")) {
				if (!validarCantidadCarecteres(numeroDocumento, 9) || !validarMaxNroCaracteres(numeroDocumento, 12) || !isNumeric(numeroDocumento)) {
					this.observacion = this.observacion + "Ingrese Nro Documento correcto,";
				}
			}
			if (!validarCantidadCarecteres(puesto, 2)) {
				this.observacion = this.observacion + "Ingrese Puesto,";
			}
			if (!validarCantidadCarecteres(correoLaboral, 3) || !validarCorreo(correoLaboral)) {
				this.observacion = this.observacion + "Ingrese un correo laboral valido,";
			}
//			if (sindicato.equalsIgnoreCase(Constantes.VACIO) || !validarCantidadCarecteres(sindicato, 2)) {
//				this.observacion = this.observacion + "Seleccione Sindicato,";
//			}

			if (observacion.length() > 1) {
				observacion = observacion.trim().substring(0, observacion.length() - 1);
				observacionResultado = observacion;
				return observacion;
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());// NOSONAR
		}
		return observacion;
	}

	public String getEliminarFila() {// NOSONAR
		observacion = "";
		flagRegistrar = true;

		tipoDocumento = StringUtils.defaultString(tipoDocumento);
		sexo = StringUtils.defaultString(sexo);
		regimenLaboral = StringUtils.defaultString(regimenLaboral);
		sindicato = StringUtils.defaultString(sindicato);
		sigla = StringUtils.defaultString(sigla);
		responsable = StringUtils.defaultString(responsable);

		try {
			if (tipoDocumento.equals(Constantes.VACIO) && sexo.equals(Constantes.VACIO)
					&& regimenLaboral.equals(Constantes.VACIO) && sigla.equals(Constantes.VACIO)
					&& responsable.equals(Constantes.VACIO)) {
				flagRegistrar = false;
				this.observacion = this.observacion + "No se seleccionaron los 6 combos";
				return observacion;
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());// NOSONAR
		}
		return observacion;

	}
	
	public void agregarObservacion(String obs) {
		if (observacion == null || observacion.trim().isEmpty()) {
			observacion = obs;
		} else {
			observacion = observacion.concat(",").concat(obs);
		}
		
		observacionResultado = observacion;
	}
}
