package pe.gob.servir.entidad.api.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.Serializable;
import java.time.LocalDate;

@SuppressWarnings("serial")
@JsonIgnoreProperties(ignoreUnknown = true)
public class ApiSeguridadUsuarioDTO implements Serializable {

	private String usuario;
	private String correoElectronico;
	private Long personaId;
	private String urlAvatar;
	private String urlSistema;
	private String firmaDeCorreo;
	private String password;
	private String codTemplateSendEmail;
	private LocalDate fechaInicioVigencia;

	public ApiSeguridadUsuarioDTO() {
		this.firmaDeCorreo = "";
	}

	public String getUsuario() {
		return usuario;
	}

	public void setUsuario(String usuario) {
		this.usuario = usuario;
	}

	public String getCorreoElectronico() {
		return correoElectronico;
	}

	public void setCorreoElectronico(String correoElectronico) {
		this.correoElectronico = correoElectronico;
	}

	public Long getPersonaId() {
		return personaId;
	}

	public void setPersonaId(Long personaId) {
		this.personaId = personaId;
	}

	public String getUrlAvatar() {
		return urlAvatar;
	}

	public void setUrlAvatar(String urlAvatar) {
		this.urlAvatar = urlAvatar;
	}

	public String getUrlSistema() {
		return urlSistema;
	}

	public void setUrlSistema(String urlSistema) {
		this.urlSistema = urlSistema;
	}

	public String getFirmaDeCorreo() {
		return firmaDeCorreo;
	}

	public void setFirmaDeCorreo(String firmaDeCorreo) {
		this.firmaDeCorreo = firmaDeCorreo;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getCodTemplateSendEmail() {
		return codTemplateSendEmail;
	}

	public void setCodTemplateSendEmail(String codTemplateSendEmail) {
		this.codTemplateSendEmail = codTemplateSendEmail;
	}

	public LocalDate getFechaInicioVigencia() {
		return fechaInicioVigencia;
	}

	public void setFechaInicioVigencia(LocalDate fechaInicioVigencia) {
		this.fechaInicioVigencia = fechaInicioVigencia;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("UsuarioBasicoItem [usuario=");
		builder.append(usuario);
		builder.append(", correoElectronico=");
		builder.append(correoElectronico);
		builder.append(", personaId=");
		builder.append(personaId);
		builder.append(", urlAvatar=");
		builder.append(urlAvatar);
		builder.append(", urlSistema=");
		builder.append(urlSistema);
		builder.append(", firmaDeCorreo=");
		builder.append(firmaDeCorreo);
		builder.append(", password=");
		builder.append(password);
		builder.append(", codTemplateSendEmail=");
		builder.append(codTemplateSendEmail);
		builder.append(", fechaInicioVigencia=");
		builder.append(fechaInicioVigencia);
		builder.append("]");
		return builder.toString();
	}

}