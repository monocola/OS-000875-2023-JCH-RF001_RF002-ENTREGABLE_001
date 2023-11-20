package pe.gob.servir.mensajeria.security;

import org.jboss.resteasy.jwt.JsonWebToken;

import lombok.Getter;

@Getter
@SuppressWarnings("serial")
public class MyJsonWebToken extends JsonWebToken {

	private Usuario usuario;
	private Aplicacion aplicacion;

	public MyJsonWebToken() {
		super();
		usuario = new Usuario();
		aplicacion = new Aplicacion();
	}

	// Superclass setters
	public void setJti(String id) {
		this.id = id;
	}

	public void setExp(long expiration) {
		this.expiration = expiration;
	}

	public void setNbf(long notBefore) {
		this.notBefore = notBefore;
	}

	public void setIat(long issuedAt) {
		this.issuedAt = issuedAt;
	}

	public void setIss(String issuer) {
		this.issuer = issuer;
	}

	public void setAud(String audience) {
		this.audience = audience;
	}

	public void setPrn(String principal) {
		this.principal = principal;
	}

	public void setTyp(String type) {
		this.type = type;
	}

	// Inner Class setters
	public MyJsonWebToken usuarioId(Long usuarioId) {
		this.usuario.usuarioId = usuarioId;
		return this;
	}

	public MyJsonWebToken usuario(String usuario) {
		this.usuario.usuario = usuario;
		return this;
	}

	public MyJsonWebToken cambiaPassword(String cambiaPassword) {
		this.usuario.cambiaPassword = cambiaPassword;
		return this;
	}

	public MyJsonWebToken correoElectronico(String correoElectronico) {
		this.usuario.correoElectronico = correoElectronico;
		return this;
	}

	public MyJsonWebToken aplicacionId(Long aplicacionId) {
		this.aplicacion.aplicacionId = aplicacionId;
		return this;
	}

	public MyJsonWebToken aplicacionNombre(String aplicacionNombre) {
		this.aplicacion.nombre = aplicacionNombre;
		return this;
	}

	public MyJsonWebToken multisesion(String multisesion) {
		this.aplicacion.multisesion = multisesion;
		return this;
	}

	/**
	 * Inner Class Usuario
	 *
	 * @author ttorres
	 */
	@Getter
	public static class Usuario {

		private Long usuarioId;
		private String usuario;
		private String cambiaPassword;
		private String correoElectronico;
	}

	/**
	 * Inner Class Aplicacion
	 *
	 * @author ttorres
	 */
	@Getter
	public static class Aplicacion {

		private Long aplicacionId;
		private String nombre;
		private String multisesion;
	}
}
