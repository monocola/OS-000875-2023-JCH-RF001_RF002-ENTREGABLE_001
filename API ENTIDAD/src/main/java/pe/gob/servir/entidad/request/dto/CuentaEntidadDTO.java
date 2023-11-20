package pe.gob.servir.entidad.request.dto;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class CuentaEntidadDTO {
	
	private Long entidadId;
	private Long personaId;	
	private Long areaId;
	private String puestoTrabajoId;
	private Long correoId;
	private Long telefonoId;
	private String motivo;
	
	
	

}
