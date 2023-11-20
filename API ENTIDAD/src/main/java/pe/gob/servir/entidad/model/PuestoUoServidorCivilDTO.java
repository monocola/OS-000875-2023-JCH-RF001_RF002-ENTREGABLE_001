package pe.gob.servir.entidad.model;

import java.time.LocalDateTime;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class PuestoUoServidorCivilDTO {

	@Id
	private Long correlativo;
	private Long detalleuoId;
	private String siglaUO;
	private String descripcionUO;
	private Long puestoId;
	private String descripcionPuesto;	
	private Integer tipoAsignacion;
	private String descTipoAsignacion;
	private LocalDateTime fechaInicio;
	private LocalDateTime fechaCese;
	private String segmento;
	private String rol;
	private String estado;
	private String uoId;
	private String motivoEncargatura;
	private String motivoId;
	private String estadoRegistro;	
	
	
	
	
	
	
}



