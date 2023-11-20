package pe.gob.servir.entidad.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Generico implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	Integer id;
	String codigoTexto;
	String descripcion;

	public Generico(Integer id, String descripcion) {
		super();
		this.id = id;
		this.descripcion = descripcion;
	}

	public Generico(Integer id, String codigoTexto, String descripcion) {
		super();
		this.id = id;
		this.codigoTexto = codigoTexto;
		this.descripcion = descripcion;
	}
	
}
