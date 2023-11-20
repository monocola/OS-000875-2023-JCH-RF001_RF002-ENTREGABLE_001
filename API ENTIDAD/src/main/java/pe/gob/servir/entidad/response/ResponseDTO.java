package pe.gob.servir.entidad.response;

import com.googlecode.jmapper.annotations.JGlobalMap;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@JGlobalMap
@Getter
@Setter
@SuppressWarnings("serial")
@AllArgsConstructor
public class ResponseDTO implements Serializable {
	String identificador;
	String cuerpo;
	String mensaje;
}
