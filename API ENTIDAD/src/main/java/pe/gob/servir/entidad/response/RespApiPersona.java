package pe.gob.servir.entidad.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.api.dto.ApiAuditoriaDTO;

@Getter
@Setter
@ToString
public class RespApiPersona {		
	  
		 private  Persona persona ; 
		 private  PersonaNatural personaNatural;
		 private  PersonaJuridica personaJuridica;
	     
		 private  List<Direccion> direcciones ;
		 private  List<Documentos> documentos;
		 private  List<Web> webs;
		 private  List<Correo> correos;
		 private  List<Telefono> telefonos;
	     private  Pais pais;
	     
		public RespApiPersona() {
			super();
			this.direcciones= new ArrayList<Direccion>();
			this.documentos= new ArrayList<Documentos>();
			this.webs= new ArrayList<Web>();
			this.correos= new ArrayList<Correo>();
			this.telefonos= new ArrayList<Telefono>();
		}
		public RespApiPersona(Persona persona,PersonaNatural personaNatural,PersonaJuridica personaJuridica,  List<Direccion> direcciones,
				List<Documentos> documentos, List<Web> webs, List<Correo> correos, List<Telefono> telefonos) {
			super();
			this.persona = persona;
			this.personaNatural = personaNatural;
			this.personaJuridica = personaJuridica;
			this.direcciones = direcciones;
			this.documentos = documentos;
			this.webs = webs;
			this.correos = correos;
			this.telefonos = telefonos;
		}
		
		@Getter
		@Setter
		@ToString
		public class Pais extends ApiAuditoriaDTO{
			private Long paisId;
			private String codSunat;
			private String nacionalidad;
			private String nombrePais;		
			
			public Pais() {
				super();
			}
		}
		
		@Getter
		@Setter
		@ToString
		public class Persona extends ApiAuditoriaDTO{
			
			private Long personaId;
			private String tipoPersona;
			private Long documentoId;
			private Long direccionId;
			private Long telefonoId;
			private Long correoId;
			private Long webId;
			private String estadoPersona; 
			
			public Persona() {
				super();
			}
		}
		@Getter
		@Setter
		@ToString
		public class PersonaJuridica extends ApiAuditoriaDTO{
			private Long personaId;
		    private String razonSocial;
		    private String nombreComercial;
		    private String fechaInscripcion;
		    private String fechaInicioActividad;
		    private String fechaBaja;
		    private String actividadEconomicaPrincipal;
		    private String estadoContribuyente;
		    private String condicionContribuyente;
		    private String validadoSunat;
		    
			public PersonaJuridica() {
				super();
			}
		}
		
		@Getter
		@Setter
		@ToString
		public class PersonaNatural extends ApiAuditoriaDTO{

			private Long personaId;
			private String nombres;
			private String apellidoPaterno;
			private String apellidoMaterno;
			private String apellidoCasada;
			private String estadoCivil;
			private String sexo;
			private String fechaNacimiento;
			private Date fechaFallecimiento;
			private String restriccionReniec;
			private String validadoReniec;
			private String validadoMigracion;	
			private Long paisId;
			
			public PersonaNatural() {
				super();
			}
			
		}
		   @Getter
		   @Setter
		   @ToString
	       public static class Documentos extends ApiAuditoriaDTO{
			
			private Long documentoId;
			private Long personaId;
			private Integer tipoDocumento;
			private String  numeroDocumento;
			private String  fechaCaducidad;		
			
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
	    public static class Correo extends ApiAuditoriaDTO{
	    	private Long correoId;
	    	private Long personaId;
	    	private String tipoCorreo;
	    	private String correo;
	   		
	   		public Correo(){
	   			super();
	   		}
	   		  		
	   	}
	    
		@Getter
		@Setter
		@ToString
	    public static class Telefono extends ApiAuditoriaDTO{
	    	private Long telefonoId;
	    	private Long personaId;
	    	private String tipoTelefono;
	    	private String codigoArea;
	    	private String numeroTelefono;
	    	private String numeroAnexo;
	    	
			public Telefono() {
				super();
			}			
	    }
	    
		 @Getter
		 @Setter
		 @ToString
	     public static class Direccion extends ApiAuditoriaDTO{
			
	    	    private Long   direccionId ;
	    		private Long personaId;
	    		private Long ubigeoId ;
	    		private UbigeoFull ubigeoFull ;
	    		private String tipoDireccion;
	    		private String direccionCompleta ;
	    		private String referencia;
	    			    		
				public Direccion() {
					super();
				}
				   
		}
	     
		 @Getter
		 @Setter
		 @ToString
	     public static class Web extends ApiAuditoriaDTO{
	    		private Long webId;
	    		private Long personaId;
	    		private String urlWeb ;
	    			    		
				public Web() {
					super();
				}
				
	 	}
	    
		@Getter
		@Setter 
		@ToString
	    public static class UbigeoFull {
	    	private String origen;
	    	private Integer departamentoUbigeoId;
	    	private String departamentoUbigeo;
	    	private String departamentoDescripcion;
	    	private Integer provinciaUbigeoId;
	    	private String provinciaUbigeo;
	    	private String provinciaDescripcion;
	    	private Integer distritoUbigeoId;
	    	private String distritoUbigeo;
	    	private String distritoDescripcion;
	    	   
			public UbigeoFull() {
				super();
			}
			 	
	    } 
	}










