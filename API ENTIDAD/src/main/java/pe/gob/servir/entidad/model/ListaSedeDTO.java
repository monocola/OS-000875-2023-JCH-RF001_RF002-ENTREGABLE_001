package pe.gob.servir.entidad.model;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class ListaSedeDTO {
	@Id
	private Long sedeId;
	private String nombreSede;
	private Long direccionId;
	private String direccion;
	private String referenciaLugar;
	private Long departamentoId;
	private String departamento;
	private Long provinciaId;
	private String provincia;
	private Long distritoId;
	private String distrito;
	private String estadoId;
	private String estado;
	private String anexo;
	private String telefono;
	private String nombreRepresentante;
	

}
