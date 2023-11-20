package pe.gob.servir.entidad.api.dto;

import java.util.Date;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class PersonaDTO {
	
	private Integer tipoDocumento;
	private String numeroDocumento;
	private String nombres;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String apellidoCasada;
	private String fechaNacimiento;
	private String sexo;
	private Integer estadoCivil;
	private String imagen;
	private Long cargoId;
	private Integer puestoTrabajoId;
	private Long paisId;
	private Long ubigeoId;	
	private String direccionCompleta;
	private String referenciaDireccion;
	private Long correoPrincipalId;
	private String correoPrincipal;
	private Long correoSecundarioId;
	private String correoSecundario;
	private String correoLaboral;
	private Long telefonoFijoId;
	private String telefonoFijo;
	private String anexo;
	private Long celularPrincipalId;
	private String celularPrincipal;
	private String celularSecundario;
	private String celularLaboral;
	private String rutaPaginaWeb;
	private String lugarNacimiento;
	
	public PersonaDTO() {
	
	}
	
	public PersonaDTO(Integer tipoDocumento,String numeroDocumento,String nombres,String apellidoPaterno,String apellidoMaterno,String apellidoCasada,String fechaNacimiento,
			String sexo,Integer estadoCivil,String imagen,Long cargoId,Integer puestoTrabajoId,Long paisId,Long ubigeoId,String direccionCompleta,
			 String referenciaDireccion,Long correoPrincipalId,String correoPrincipal,Long correoSecundarioId,String correoSecundario,String correoLaboral,
			 Long telefonoFijoId,String telefonoFijo,Long celularPrincipalId,String celularPrincipal,String celularSecundario,String celularLaboral,
			 String rutaPaginaWeb,String lugarNacimiento) {
		 this.tipoDocumento =  tipoDocumento;
		 this.numeroDocumento =  numeroDocumento;
		 this.nombres =  nombres;
		 this.apellidoPaterno =  apellidoPaterno;
		 this.apellidoMaterno =  apellidoMaterno;
		 this.apellidoCasada =  apellidoCasada;
		 this.fechaNacimiento =  fechaNacimiento;
		 this.sexo =  sexo;
		 this.estadoCivil =  estadoCivil;
		 this.imagen =  imagen;
		 this.cargoId =  cargoId;
		 this.puestoTrabajoId = puestoTrabajoId ;
		 this.paisId = paisId ;
		 this.ubigeoId =  ubigeoId;	
		 this.direccionCompleta = direccionCompleta ;
		 this.referenciaDireccion =  referenciaDireccion;
		 this.correoPrincipalId =  correoPrincipalId;
		 this.correoPrincipal =  correoPrincipal;
		 this.correoSecundarioId = correoSecundarioId ;
		 this.correoSecundario =  correoSecundario;
		 this.correoLaboral =  correoLaboral;
		 this.telefonoFijoId =  telefonoFijoId;
		 this.telefonoFijo = telefonoFijo ;		 
		 this.celularPrincipalId = celularPrincipalId ;
		 this.celularPrincipal =celularPrincipal  ;
		 this.celularSecundario =celularSecundario  ;
		 this.celularLaboral = celularLaboral ;
		 this.rutaPaginaWeb = rutaPaginaWeb ;
		 this.lugarNacimiento = lugarNacimiento ;
	}
	
	public PersonaDTO(Integer tipoDocumento,String numeroDocumento,String nombres,String apellidoPaterno,String apellidoMaterno,String apellidoCasada,
			String sexo,Integer estadoCivil,String imagen,Long cargoId,Long paisId,Long ubigeoId,String direccionCompleta,
			Long correoPrincipalId,String correoPrincipal,Long correoSecundarioId,String correoSecundario,String correoLaboral,
			Long telefonoFijoId,String telefonoFijo,String anexo,Long celularPrincipalId,String celularPrincipal,String celularSecundario,String celularLaboral,
			 String rutaPaginaWeb) {
		 this.tipoDocumento =  tipoDocumento;
		 this.numeroDocumento =  numeroDocumento;
		 this.nombres =  nombres;
		 this.apellidoPaterno =  apellidoPaterno;
		 this.apellidoMaterno =  apellidoMaterno;
		 this.apellidoCasada =  apellidoCasada;
		 this.sexo =  sexo;
		 this.estadoCivil =  estadoCivil;
		 this.imagen =  imagen;
		 this.cargoId =  cargoId;
		 this.paisId = paisId ;
		 this.ubigeoId =  ubigeoId;	
		 this.direccionCompleta = direccionCompleta ;
		 this.correoPrincipalId =  correoPrincipalId;
		 this.correoPrincipal =  correoPrincipal;
		 this.correoSecundarioId = correoSecundarioId ;
		 this.correoSecundario =  correoSecundario;
		 this.correoLaboral =  correoLaboral;
		 this.telefonoFijoId =  telefonoFijoId;
		 this.telefonoFijo = telefonoFijo ;		
		 this.anexo = anexo ;
		 this.celularPrincipalId = celularPrincipalId ;
		 this.celularPrincipal =celularPrincipal  ;
		 this.celularSecundario =celularSecundario  ;
		 this.celularLaboral = celularLaboral ;
		 this.rutaPaginaWeb = rutaPaginaWeb ;
	}
}
