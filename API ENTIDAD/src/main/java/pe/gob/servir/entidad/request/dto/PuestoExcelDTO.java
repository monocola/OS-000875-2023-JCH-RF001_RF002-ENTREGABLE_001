package pe.gob.servir.entidad.request.dto;

import static pe.gob.servir.entidad.util.ParametrosUtil.validarCantidadCarecteres;
import static pe.gob.servir.entidad.util.ParametrosUtil.validarMaxNroCaracteres;

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
public class PuestoExcelDTO{

	@NotNull(message = Constantes.CAMPO + " " + Constantes.ENTIDADID + " " + Constantes.ES_OBLIGATORIO)
	private String entidadId;

	private Long unidadOrganicaId;
	
	@Size(max = 10, message = Constantes.CAMPO + " sigla " + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String sigla;
	
	@Size(max = 500, message = Constantes.CAMPO + " puesto " + Constantes.ES_INVALIDO + ", máximo 500 "
			+ Constantes.CARACTERES)
	private String puesto;
	
	@NotNull(message = Constantes.CAMPO + "responsable " + Constantes.ES_OBLIGATORIO)
	private String responsable;
	
	private String observacion;

	private String filaObservacion;
	private boolean flagRegistrar;
	private String observacionResultado;
	
	public String getAttribute(String cabecera) {
		String attribute = "";
		switch (cabecera.toUpperCase()) {
			case "SIGLA ORGANO, UO, SUB UO":
				attribute = "sigla";
				break;
			case "PUESTO":
				attribute = "puesto";
				break;
			case "RESPONSABLE DE ORGANO / UO":
				attribute = "responsable";
				break;
			default:
				break;
		}
		return attribute;
	}
	
	public String getObservacion() {// NOSONAR
		observacion = "";
		flagRegistrar = true;
		
		sigla = StringUtils.defaultString(sigla);
		responsable = StringUtils.defaultString(responsable);
		puesto = StringUtils.defaultString(puesto);
	
		try {
			if (sigla.equals(Constantes.SELECCIONAR)
					&& responsable.equals(Constantes.SELECCIONAR)
					&& StringUtils.isEmpty(puesto)) {
				flagRegistrar = false;
				return observacion;
			}

			if (sigla.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(sigla, 1)) {
				this.observacion = this.observacion + "Seleccione Sigla,";
			}
			if (responsable.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(responsable, 1)) {
				this.observacion = this.observacion + "Seleccione si es Jefe,";
			}			
			
			if (!validarCantidadCarecteres(puesto, 2)) {
				this.observacion = this.observacion + "Ingrese Puesto,";
			}
			if(!validarMaxNroCaracteres(puesto, 500)) {
				this.observacion = this.observacion + "La cantidad máxima de caracteres es 500,";
			}

			if (observacion.length() > 1) {
				observacion = observacion.trim().substring(0, observacion.length() - 1);
				this.observacionResultado = observacion;
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

		sigla = StringUtils.defaultString(sigla);
		responsable = StringUtils.defaultString(responsable);
		
		try {
			if (sigla.equals(Constantes.SELECCIONAR) 
					&& responsable.equals(Constantes.SELECCIONAR)) {
				flagRegistrar = false;
				this.observacion = this.observacion + "No se seleccionaron los combos";
				return observacion;
			}
			if (responsable.equals(Constantes.VACIO)
					&& sigla.equals(Constantes.VACIO)) {
				flagRegistrar = false;
				this.observacion = this.observacion + "No se seleccionaron los combos";
				return observacion;
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		return observacion;

	}
}
