package pe.gob.servir.entidad.common;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum ErrorAuthentication {
	
	BASIC_AUTH_NULO("BASAN", ErrorAuthentication.MENSAJE_401),
	APLICACION_NO_EXISTE("APNEX", ErrorAuthentication.MENSAJE_401),
	BEARER_AUTH_NULO("BEAAN", ErrorAuthentication.MENSAJE_401),
	JWT_INVALIDO("JWTIN", "token inválido"),
	JWT_EXPIRADO("JWTEX", "token expirado");
	
	private static final String MENSAJE_401 = "autenticación es requerida para acceder al recurso";
	
	private String codigo;
	private String mensaje;
	
}
