package pe.gob.servir.entidad.api.dto;

import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.ser.InstantSerializer;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ApiActualizarEstadoRolUsuario {

	private Long usuarioRolId;
	private String estado;
	private String fechaInicioVigencia;
	private String fechaFinVigencia;
	
    
}
