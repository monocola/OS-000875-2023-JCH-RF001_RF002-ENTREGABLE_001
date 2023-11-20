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
public class OrganigramaExcelDTO {

	@NotNull(message = Constantes.CAMPO + " entidadId" + Constantes.ES_OBLIGATORIO)
	private String entidadId;

	@NotNull(message = Constantes.CAMPO + " tipoOrgano" + Constantes.ES_OBLIGATORIO)
	private String tipoOrgano;

	@NotNull(message = Constantes.CAMPO + " nombreOrgano" + Constantes.ES_OBLIGATORIO)
	@Size(max = 100, message = Constantes.CAMPO + " nombreOrgano" + Constantes.ES_INVALIDO + ", máximo 100"
			+ Constantes.CARACTERES)
	private String nombreOrgano; // Organo, Unidad Organica, Sub Unidad Organica

	@NotNull(message = Constantes.CAMPO + " sigla" + Constantes.ES_OBLIGATORIO)
	@Size(max = 10, message = Constantes.CAMPO + " sigla" + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String siglas;

	@NotNull
	@Size(max = 10, message = Constantes.CAMPO + " siglaOrganoSuperior" + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String siglasOrganoSuperior;

	private String observacion;
	private String filaObservacion;
	private boolean flagRegistrar;
	private String observacionResultado;

	public String getAttribute(String cabecera) {
		String attribute = "";

		switch (cabecera.toUpperCase()) {
		case "TIPO DE ORGANO":
			attribute = "tipoOrgano";
			break;
		case "ORGANO / UNIDAD ORGANICA / SUB UNIDAD ORGANICA":
			attribute = "nombreOrgano";
			break;
		case "SIGLAS":
			attribute = "siglas";
			break;
		case "SIGLAS ORGANO / UNIDAD ORGANICA SUPERIOR":
			attribute = "siglasOrganoSuperior";
			break;
		default:
			break;
		}
		return attribute;
	}

	public String getObservacion() {
		observacion = "";
		flagRegistrar = true;
		
		tipoOrgano = StringUtils.defaultString(tipoOrgano);
		nombreOrgano = StringUtils.defaultString(nombreOrgano);
		siglas = StringUtils.defaultString(siglas);
		siglasOrganoSuperior = StringUtils.defaultString(siglasOrganoSuperior);		
		
		try {
			if (tipoOrgano.equals(Constantes.SELECCIONAR) && StringUtils.isEmpty(nombreOrgano) && StringUtils.isEmpty(siglas) && 
					StringUtils.isEmpty(siglasOrganoSuperior)) {
				flagRegistrar = false;
				return observacion + "seleccione el tipo de organo, Ingrese Nombre del Organo/Unidad Organica/Sub Unidad Organica, Ingrese Sigla, Ingrese la Sigla de la UO superior";
			}else {
				if(tipoOrgano.equalsIgnoreCase(Constantes.SELECCIONAR) || !validarCantidadCarecteres(tipoOrgano, 2)) {
					this.observacion = this.observacion + "Seleccione Tipo Organo,";
				}
				if(!validarCantidadCarecteres(nombreOrgano, 2)) {
					this.observacion = this.observacion + "Ingrese Nombre del Organo, Unidad Organica o Sub Unidad Organica,";
				}
				if(!validarMaxNroCaracteres(nombreOrgano, 300)) {
					this.observacion = this.observacion + "El Nombre del Organo, UO o Sub UO no debe superar los 300 caracteres,";
				}
				if(!validarCantidadCarecteres(siglas, 2)) {
					this.observacion = this.observacion + "Ingrese Sigla,";
				}
				if(!validarCantidadCarecteres(siglasOrganoSuperior, 2)) {
					if(!siglasOrganoSuperior.equalsIgnoreCase("NO APLICA")) {
						this.observacion = this.observacion + "Ingrese la Sigla de la UO superior,";
					}				
				}
			}
			if (observacion.length() > 1) {
				observacion = observacion.trim().substring(0, observacion.length() - 1);
				observacionResultado = observacion;
				return observacion;
			}
			
			

		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		return observacion;
	}

	public String getEliminarFila() {
		observacion="";
		flagRegistrar = true;		
		StringUtils.defaultString(tipoOrgano);	
		
		try {
			if(tipoOrgano.equals(Constantes.SELECCIONAR) || tipoOrgano.equals(Constantes.VACIO)) {
				flagRegistrar=false;
				this.observacion=this.observacion+"No se ha seleccionado ningun elemento de la lista";
				return observacion;
			}					
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}		
		return observacion;
	}
}
