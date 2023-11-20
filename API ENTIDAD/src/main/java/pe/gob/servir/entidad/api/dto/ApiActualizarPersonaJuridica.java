package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiActualizarPersonaJuridica {

	private Long personaId;
	private String nombreComercial;
	private String razonSocial;
	private String ruc;
}
