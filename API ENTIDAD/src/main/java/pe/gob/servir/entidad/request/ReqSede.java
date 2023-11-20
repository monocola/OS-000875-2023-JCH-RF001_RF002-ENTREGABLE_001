package pe.gob.servir.entidad.request;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;

@Getter
@Setter
public class ReqSede {
	

	@NotNull(message = Constantes.CAMPO + " entidadId " + Constantes.ES_OBLIGATORIO)
	private Long entidadId;
	
	private Long padreSedeId;
	
	@NotNull(message = Constantes.CAMPO + " direccion " + Constantes.ES_OBLIGATORIO)
	@Size(max = 100, message = Constantes.CAMPO + " direccion " + Constantes.ES_INVALIDO + ", máximo 100 "
			+ Constantes.CARACTERES)
	private String direccion;
	
	@NotNull(message = Constantes.CAMPO + " ubigeo " + Constantes.ES_OBLIGATORIO)
	private Integer ubigeo; 
	
	@NotNull(message = Constantes.CAMPO + " nombreSede " + Constantes.ES_OBLIGATORIO)
	@Size(max = 100, message = Constantes.CAMPO + " nombreSede " + Constantes.ES_INVALIDO + ", máximo 100 "
			+ Constantes.CARACTERES)
	private String nombreSede;
	
	@Size(max = 10, message = Constantes.CAMPO + " ambito " + Constantes.ES_INVALIDO + ", máximo 10 "
			+ Constantes.CARACTERES)
	private String ambito;
	
	@NotNull(message = Constantes.CAMPO + " estadoRegistro " + Constantes.ES_OBLIGATORIO)
	private String estadoRegistro;
	
	@Size(max = 20, message = Constantes.CAMPO + " anexo " + Constantes.ES_INVALIDO + ", máximo 20 "
			+ Constantes.CARACTERES)
	private String anexo;
	
	@Size(max = 20, message = Constantes.CAMPO + " telefono " + Constantes.ES_INVALIDO + ", máximo 20 "
			+ Constantes.CARACTERES)
	private String telefono;
	
	@Size(max = 100, message = Constantes.CAMPO + " representante " + Constantes.ES_INVALIDO + ", máximo 100 "
			+ Constantes.CARACTERES)
	private String representante;

}
