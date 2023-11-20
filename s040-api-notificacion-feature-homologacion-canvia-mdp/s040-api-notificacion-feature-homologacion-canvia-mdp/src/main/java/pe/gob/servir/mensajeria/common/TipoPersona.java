package pe.gob.servir.mensajeria.common;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum TipoPersona {

	NATURAL("NAT"), 
	JURIDICA("JUR");

	private String codigo;

}
