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
public class Parametro {
	
	@Id
	private Integer parametroId;
	private String tipoParametro;
	private Integer codigoNumero;
	private String codigoTexto;
	private String valorTexto;
	private Integer orden;
	private String descripcion;
	// @Override
	// public String toString() {
	// 	return "Parametro [parametroId=" + parametroId + ", tipoParametro=" + tipoParametro + ", codigoNumero="
	// 			+ codigoNumero + ", codigoTexto=" + codigoTexto + ", valorTexto=" + valorTexto + ", orden=" + orden
	// 			+ ", descripcion=" + descripcion + "]";
	// }
}
