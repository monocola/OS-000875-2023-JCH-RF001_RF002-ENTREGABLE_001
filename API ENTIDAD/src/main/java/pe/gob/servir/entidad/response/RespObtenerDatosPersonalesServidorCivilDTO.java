package pe.gob.servir.entidad.response;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RespObtenerDatosPersonalesServidorCivilDTO implements Serializable{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1910799219154957762L;
	private RespPersonaServidorCivil datosPersonalesServidorCivil;

}
