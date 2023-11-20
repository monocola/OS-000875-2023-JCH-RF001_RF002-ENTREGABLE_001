package pe.gob.servir.entidad.request.dto;

import static pe.gob.servir.entidad.util.ParametrosUtil.validarCantidadCarecteres;
import static pe.gob.servir.entidad.util.ParametrosUtil.validarEmail;
import static pe.gob.servir.entidad.util.ParametrosUtil.validarParametroNumericoValor;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import org.apache.commons.lang3.StringUtils;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@JGlobalMap
@Getter
@Setter
public class UnidadOrganicaExcelDTO {
	@NotNull(message = Constantes.CAMPO + " entidadId " + Constantes.ES_OBLIGATORIO)
	private String entidadId;
	@NotNull(message = Constantes.CAMPO + " estado " + Constantes.ES_OBLIGATORIO)
	private String estado;
	private String nivel;
	@NotNull(message = Constantes.CAMPO + " nombreOrgano " + Constantes.ES_OBLIGATORIO)
	@Size(max = 200, message = Constantes.CAMPO + " organo " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String nombreOrgano;
	@Size(max = 10, message = Constantes.CAMPO + " sigla " + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String sigla;	
	private String tipoOrgano;
	private String tipoDocumento;
	@Size(max = 30, message = Constantes.CAMPO + " numeroDocumento " + Constantes.ES_INVALIDO + ", máximo 30 "
			+ Constantes.CARACTERES)
	private String nroDocumento;
	@Size(max = 200, message = Constantes.CAMPO + " nombres " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String nombres;
	@Size(max = 200, message = Constantes.CAMPO + " apellidoPaterno " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String apellidoPaterno;
	@Size(max = 200, message = Constantes.CAMPO + " apellidoMaterno " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String apellidoMaterno;
	@Size(max = 200, message = Constantes.CAMPO + " puesto " + Constantes.ES_INVALIDO + ", máximo 200 "
			+ Constantes.CARACTERES)
	private String puesto;
	@Size(max = 25, message = Constantes.CAMPO + " celular " + Constantes.ES_INVALIDO + ", máximo 25 "
			+ Constantes.CARACTERES)
	private String celular;
	@Size(max = 150, message = Constantes.CAMPO + " correoLaboral " + Constantes.ES_INVALIDO + ", máximo 150 "
			+ Constantes.CARACTERES)
	@Email(message = Constantes.CAMPO + " correo " + Constantes.ES_INVALIDO + "")
	private String correoLaboral;
	private String observacion;
	private String pais;
	
	private String filaObservacion;	
	private boolean flagRegistrar;
	private String observacionResultado;
	
	public String getAttribute(String cabecera){
		String attribute = "";
		
		
		switch (cabecera.toUpperCase()) {
			case "ESTADO":
				attribute = "estado";
				break;
			case "NIVEL":
				attribute = "nivel";		
				break;
			case "NOMBRE UNIDAD ORGANICA":
				attribute = "nombreOrgano";	
				break;
			case "SIGLA":
				attribute = "sigla";	
				break;			
			case "DEPENDENCIA JERARQUICA":
				attribute = "tipoOrgano";	
				break;	
			case "TIPO DOCUMENTO":
				attribute = "tipoDocumento";	
				break;
			case "NRO IDENTIDAD":
				attribute = "nroDocumento";	
				break;
			case "NOMBRES":
				attribute = "nombres";				
				break;
			case "APELLIDO PATERNO":
				attribute = "apellidoPaterno";
				break;
			case "APELLIDO MATERNO":
				attribute = "apellidoMaterno";
				break;
			case "PUESTO":
				attribute = "puesto";
				break;
			case "CELULAR":
				attribute = "celular";
				break;
			case "CORREO LABORAL":
				attribute = "correoLaboral";
				break;
			case "PAISES":
				attribute = "pais";
				break;			
			default:
				break;
		}
		return attribute;
	}
	
	public String getObservacion() {//NOSONAR
		observacion = "";
		flagRegistrar= true;
		correoLaboral = correoLaboral!=null ? correoLaboral.toLowerCase() : correoLaboral;
		
		try {			   
				if(estado.equals(Constantes.SELECCIONAR) && nivel.equals(Constantes.SELECCIONAR) && StringUtils.isEmpty(nombreOrgano) && StringUtils.isEmpty(sigla) &&
						tipoOrgano.equals(Constantes.SELECCIONAR) && tipoDocumento.equals(Constantes.SELECCIONAR) && StringUtils.isEmpty(nroDocumento) && StringUtils.isEmpty(nombres) && StringUtils.isEmpty(apellidoPaterno) && StringUtils.isEmpty(apellidoMaterno)  && 
						StringUtils.isEmpty(puesto) && StringUtils.isEmpty(celular) && StringUtils.isEmpty(correoLaboral)&& pais.equals(Constantes.SELECCIONAR)){
					flagRegistrar = false;
					return observacion;
				}
			
				if (estado.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(estado, 2)) {
					this.observacion = this.observacion + "Seleccione Estado,";
				}
				if (nivel.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(nivel, 3)) {
					this.observacion = this.observacion + "Seleccione Nivel,";
				}	
				if (!validarCantidadCarecteres(nombreOrgano, 2)) {
					this.observacion = this.observacion + "Ingrese Nombre del Organo,";
				}
				if (!validarCantidadCarecteres(sigla, 2)) {
					this.observacion = this.observacion + "Ingrese Sigla,";
				}
				
				if (tipoOrgano.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(tipoOrgano, 4)) {
					this.observacion = this.observacion + "Seleccione la Dependencia Jerarquica,";
				}
				if(tipoDocumento.equals("1 - DNI") && !validarCantidadCarecteres(apellidoMaterno, 2)){
					this.observacion = this.observacion + "Ingrese  Apellido Materno,";									
				}			
				if (tipoDocumento.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(tipoDocumento, 2)) {
					this.observacion = this.observacion + "Seleccione Tipo de Documento,";
				}
				if (!validarCantidadCarecteres(nroDocumento, 8)) {
					this.observacion = this.observacion + "Ingrese Nro Documento,";
				}
				if (!validarCantidadCarecteres(nombres, 2)) {
					this.observacion = this.observacion + "Ingrese Nombre,";
				}
				if (!validarCantidadCarecteres(apellidoPaterno, 2)) {
					this.observacion = this.observacion + "Ingrese Apellido Paterno,";
				}				
				if (!validarCantidadCarecteres(puesto, 2)) {
					this.observacion = this.observacion + "Ingrese Puesto,";
				}				
				String rptTelef = validarParametroNumericoValor(celular,"Celular");
				if (!rptTelef.equals(Constantes.CONSTANTE_OK)) {
					this.observacion = this.observacion +rptTelef+",";
				}
			
				if (!validarEmail(correoLaboral)) {
					this.observacion = this.observacion + "Ingrese Correo Laboral";
				}
				if (pais.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(pais, 2)) {
					this.observacion = this.observacion + "Seleccione Pais,";
				}
			
			if (observacion.length() > 1) {
				observacion = observacion.trim().substring(0, observacion.length() - 1);
				observacionResultado = observacion;				
				return observacion;
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());//NOSONAR
		}
		return observacion;

	}
	
	public String getEliminarFila() {//NOSONAR
		observacion = "";
		flagRegistrar= true;		
		StringUtils.defaultString(estado);
		StringUtils.defaultString(nivel);
		StringUtils.defaultString(tipoOrgano);
		StringUtils.defaultString(tipoDocumento);
		StringUtils.defaultString(pais);
		try {
			
			if(estado.equals(Constantes.SELECCIONAR) && nivel.equals(Constantes.SELECCIONAR) && tipoOrgano.equals(Constantes.SELECCIONAR) &&
						 tipoDocumento.equals(Constantes.SELECCIONAR) && pais.equals(Constantes.SELECCIONAR)){
					flagRegistrar = false;
					this.observacion = this.observacion + "No se seleccionaron los 5 combos";
					return observacion;
			}
			if(estado.equals(Constantes.VACIO) && nivel.equals(Constantes.VACIO) && tipoOrgano.equals(Constantes.VACIO) &&
					 tipoDocumento.equals(Constantes.VACIO) && pais.equals(Constantes.VACIO)){
				flagRegistrar = false;
				this.observacion = this.observacion + "No se seleccionaron los 5 combos";
				return observacion;
			}
			
			
		} catch (Exception e) {
			System.out.println(e.getMessage());//NOSONAR
		}
		return observacion;

	}

}
