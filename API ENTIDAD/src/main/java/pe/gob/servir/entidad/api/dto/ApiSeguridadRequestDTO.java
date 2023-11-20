package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiSeguridadRequestDTO {

	private String usuario;
	private String correoElectronico;
	private Long personaId;
	private String urlAvatar;
	private String codTemplateSendEmail;
	private String password;
 
	public ApiSeguridadRequestDTO(String usuario, String correoElectronico, Long personaId,
			String codTemplateSendEmail, String password) {
		super();
		this.usuario = usuario;
		this.correoElectronico = correoElectronico;
		this.personaId = personaId;
		this.codTemplateSendEmail = codTemplateSendEmail;
		this.password = password;
	}

}
