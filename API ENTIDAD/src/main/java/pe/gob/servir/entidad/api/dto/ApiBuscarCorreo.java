package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiBuscarCorreo<T> {

  private Long correoId;
  private String tipoCorreo;
  private String correo;
	
}
