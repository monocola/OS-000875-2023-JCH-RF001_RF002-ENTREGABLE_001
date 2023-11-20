package pe.gob.servir.entidad.api.dto;

import java.time.LocalDate;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiActualizarPersonaNatural {

	private Long personaId;
	private LocalDate fechaNacimiento;
	private LocalDate fechaFallecimiento;
	private long paisId;
	
}
