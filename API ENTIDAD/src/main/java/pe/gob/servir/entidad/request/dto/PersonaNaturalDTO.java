package pe.gob.servir.entidad.request.dto;

import java.util.Date;

import javax.validation.constraints.Email;
import javax.validation.constraints.Size;

import com.googlecode.jmapper.annotations.JGlobalMap;

import lombok.Getter;
import lombok.Setter;

@JGlobalMap
@Getter
@Setter
public class PersonaNaturalDTO {	
	
	private Integer tipoPersona;
	private Integer tipoDocumento;
	@Size(max = 30, message = "Campo numeroDocumento es inválido, máximo 30 caracteres")
	private String numeroDocumento;	
	@Size(max = 200, message = "Campo nombres es inválido, máximo 200 caracteres")
	private String nombres;
	@Size(max = 200, message = "Campo apellidoPaterno es inválido, máximo 200 caracteres")
	private String apellidoPaterno;
	@Size(max = 200, message = "Campo apellidoMaterno es inválido, máximo 200 caracteres")
	private String apellidoMaterno;
	@Size(max = 200, message = "Campo apellidoCasada es inválido, máximo 200 caracteres")
	private String apellidoCasada;
	private Date fechaNacimiento;
	@Size(max = 1, message = "Campo sexo es inválido, máximo 1 caracter")
	private String sexo;
	@Size(max = 15, message = "Campo anexo es inválido, máximo 15 caracteres")
	private String anexo;
	private Integer estadoCivil;	
	private Long paisId;	
	@Size(max = 200, message = "Campo direccionCompleta es inválido, máximo 200 caracteres")
	private String direccionCompleta;
	@Size(max = 200, message = "Campo referenciaDireccion es inválido, máximo 200 caracteres")
	private String referenciaDireccion;
	@Size(max = 150, message = "Campo correoPrincipal es inválido, máximo 150 caracteres")
	@Email(message = "Campo correoPrincipal es inválido")
	private String correoPrincipal;
	@Size(max = 150, message = "Campo correoSecundario es inválido, máximo 150 caracteres")
	@Email(message = "Campo correoSecundario es inválido")
	private String correoSecundario;
	@Size(max = 150, message = "Campo correoLaboral es inválido, máximo 150 caracteres")
	@Email(message = "Campo correoLaboral es inválido")
	private String correoLaboral;
	@Size(max = 15, message = "Campo telefonoFijo es inválido, máximo 15 caracteres")
	private String telefonoFijo;
	@Size(max = 25, message = "Campo celularPrincipal es inválido, máximo 25 caracteres")
	private String celularPrincipal;
	@Size(max = 25, message = "Campo celularSecundario es inválido, máximo 25 caracteres")
	private String celularSecundario;
	@Size(max = 25, message = "Campo celularLaboral es inválido, máximo 25 caracteres")
	private String celularLaboral;
	@Size(max = 200, message = "Campo rutaPaginaWeb es inválido, máximo 200 caracteres")
	private String rutaPaginaWeb;

}
