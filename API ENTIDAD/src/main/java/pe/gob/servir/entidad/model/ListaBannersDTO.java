package pe.gob.servir.entidad.model;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ListaBannersDTO {
	private Long bannerId;
	private Long orden;
	private String descripcion;
	private String urlImg;
	private String urlWeb;
}
