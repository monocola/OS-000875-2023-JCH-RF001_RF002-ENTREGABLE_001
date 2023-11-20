package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;

import javax.persistence.Transient;
import javax.validation.constraints.Email;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PerfilUsuarioEdicionDTO implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long entidadId;
	
	private Integer personaId;
	
	private String telefono;
	
	@Size(max = 150, message = "Campo correo electronico institucional es obligatorio, máximo 150 caracteres")
	@Email(message = "Campo correo electronico institucional es inválido")
	private String correoElectronicoInstitucional;
	
	@Size(max = 150, message = "Campo correo electronico alterno es obligatorio, máximo 150 caracteres")
	@Email(message = "Campo correo electronico alterno es inválido")
	private String correoElectronicoAlternativo;
	
	@Size(max = 1, message = "Campo sindicatoFlag es obligatorio, máximo 1 caracter")
	private String sindicatoFlag;
	
	private String usuario;
	
	@Transient
	private Long longPersonaId;
	
	private LogoDTO foto;
	
	private Long regimenLaboralId;
	
}
