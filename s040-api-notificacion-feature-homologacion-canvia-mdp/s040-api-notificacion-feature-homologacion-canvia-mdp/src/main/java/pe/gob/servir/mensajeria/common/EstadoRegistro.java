package pe.gob.servir.mensajeria.common;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum EstadoRegistro {

	ACTIVO("1"), 
	INACTIVO("0"), 
	PENDIENTE_ACTIVO("a"), 
	PENDIENTE_INACTIVO("i");

	private String codigo;

}
