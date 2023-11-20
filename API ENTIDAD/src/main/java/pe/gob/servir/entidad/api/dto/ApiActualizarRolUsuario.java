package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiActualizarRolUsuario {
	
	private Long usuarioRolId;
	private String fechaInicioVigencia;
    private String fechaFinVigencia;
    private String estado;
    
    
}
