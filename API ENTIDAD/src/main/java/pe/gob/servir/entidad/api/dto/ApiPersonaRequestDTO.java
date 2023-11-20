package pe.gob.servir.entidad.api.dto;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiPersonaRequestDTO<T> {
	
	T persona;
	List<Direccion> direcciones;
	List<Documentos> documentos;
	List<Web> webs;
	List<Correo> correos;
	List<Telefono> telefonos;

	public ApiPersonaRequestDTO() {
		
		this.direcciones= new ArrayList<Direccion>();
		this.documentos= new ArrayList<Documentos>();
		this.webs= new ArrayList<Web>();
		this.correos= new ArrayList<Correo>();
		this.telefonos= new ArrayList<Telefono>();		
	}

	@Getter
	@Setter
	@ToString
	public static class PersonaNatural {

		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private String sexo;
		private Long paisId;
		private LocalDate fechaNacimiento;

		public PersonaNatural() {
			//constructor vacio
		}
	}
	
	@Getter
	@Setter
	@ToString
	public static class PersonaJuridica {

		private String razonSocial;
		private String nombreComercial;

		public PersonaJuridica() {
			//constructor vacio
		}
		
	}

	@Getter
	@Setter
	@ToString
	public static class Direccion {

		private Long ubigeoId;
		private String tipoDireccion;
		private String direccionCompleta;
		private String referencia;

		public Direccion(Long ubigeoId, String tipoDireccion, String direccionCompleta, String referencia) {
			super();
			this.ubigeoId = ubigeoId;
			this.tipoDireccion = tipoDireccion;
			this.direccionCompleta = direccionCompleta;
			this.referencia = referencia;
		}
	}
	@Getter
	@Setter
	@ToString
	public static class Documentos {

		private Long documentoId;
		private Integer tipoDocumento;
		private String numeroDocumento;

		public Documentos() {
			super();
		}

		public Documentos(Integer tipoDocumento, String numeroDocumento) {
			super();
			this.tipoDocumento = tipoDocumento;
			this.numeroDocumento = numeroDocumento;
		}

		public Documentos(Long documentoId, Integer tipoDocumento, String numeroDocumento) {
			super();
			this.documentoId = documentoId;
			this.tipoDocumento = tipoDocumento;
			this.numeroDocumento = numeroDocumento;
		}		
	}
	@Getter
	@Setter
	@ToString
	public static class Web {
		private String urlWeb;

		public Web(String urlWeb) {
			super();
			this.urlWeb = urlWeb;
		}
	}
	@Getter
	@Setter
	@ToString
	public static class Correo {
		private Long correoId;
		private String tipoCorreo;
		private String correo;

		public Correo() {
			super();
		}

		public Correo(Long correoId, String tipoCorreo, String correo) {
			super();
			this.correoId = correoId;
			this.tipoCorreo = tipoCorreo;
			this.correo = correo;
		}

		public Correo(String tipoCorreo, String correo) {
			super();
			this.tipoCorreo = tipoCorreo;
			this.correo = correo;
		}		

	}
	
	@Getter
	@Setter
	@ToString
	public static class Telefono {
		private Long telefonoId;
		private String tipoTelefono;
		private String codigoArea;
		private String numeroTelefono;
		private String numeroAnexo;

		public Telefono(Long telefonoId, String tipoTelefono, String codigoArea, String numeroTelefono,
				String numeroAnexo) {
			super();
			this.telefonoId = telefonoId;
			this.tipoTelefono = tipoTelefono;
			this.codigoArea = codigoArea;
			this.numeroTelefono = numeroTelefono;
			this.numeroAnexo = numeroAnexo;
		}

		public Telefono(String tipoTelefono, String codigoArea, String numeroTelefono, String numeroAnexo) {
			super();
			this.tipoTelefono = tipoTelefono;
			this.codigoArea = codigoArea;
			this.numeroTelefono = numeroTelefono;
			this.numeroAnexo = numeroAnexo;
		}
		
		public Telefono() {
			//contructor vacio
		}
		
	}
}
