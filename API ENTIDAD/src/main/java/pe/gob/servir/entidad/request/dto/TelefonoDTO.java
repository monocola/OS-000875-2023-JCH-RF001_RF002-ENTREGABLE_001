package pe.gob.servir.entidad.request.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TelefonoDTO {
	private Long telefonoId;
	private Long personaId;
	private String tipoTelefono;
	private String codigoArea;
	private String numeroTelefono;
	private String numeroAnexo;
	
	public TelefonoDTO() {
		super();
	}			

}
