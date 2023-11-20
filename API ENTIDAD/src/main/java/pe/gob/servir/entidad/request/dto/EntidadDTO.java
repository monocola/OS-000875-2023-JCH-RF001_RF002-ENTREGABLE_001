package pe.gob.servir.entidad.request.dto;

import java.util.Date;

import javax.validation.constraints.Size;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class EntidadDTO {
	
	private Integer personaId;
	private Integer adminTalentoId;
	private Date fechaAlta;
	private Integer ubigeoId;
	private Integer nivelGobiernoId;
	private Integer sectorId;
	private Integer tipoEntidadId;
	@Size(max = 1, message = "Campo comparteRuc es inválido, máximo 1 caracter")
	private String comparteRuc;
	@Size(max = 100, message = "Campo descripcionEntidad es inválido, máximo 100 caracteres")
	private String descripcionEntidad;
	@Size(max = 500, message = "Campo urLogoEntidad es inválido, máximo 500 caracteres")
	private String urLogoEntidad;
	@Size(max = 500, message = "Campo urlWebEntidad es inválido, máximo 500 caracteres")
	private String urlWebEntidad;
	private Integer direccionId;
	private String sigla;	
	
	private Integer nrosSindicatos;

	private String direccion;

	@Size(max = 500, message = "Campo urPortadaEntidad es inválido, máximo 500 caracteres")
	private String urPortadaEntidad;

	@Override
	public String toString() {
		return "EntidadDTO [personaId=" + personaId + ", adminTalentoId=" + adminTalentoId + ", fechaAlta=" + fechaAlta
				+ ", ubigeoId=" + ubigeoId + ", nivelGobiernoId=" + nivelGobiernoId + ", sectorId=" + sectorId
				+ ", tipoEntidadId=" + tipoEntidadId + ", comparteRuc=" + comparteRuc + ", descripcionEntidad="
				+ descripcionEntidad + ", urLogoEntidad=" + urLogoEntidad + ", urlWebEntidad=" + urlWebEntidad
				+ ", direccionId=" + direccionId + ", sigla=" + sigla + ", direccion=" + direccion
				+ ", urPortadaEntidad=" + urPortadaEntidad + "]";
	}
}
