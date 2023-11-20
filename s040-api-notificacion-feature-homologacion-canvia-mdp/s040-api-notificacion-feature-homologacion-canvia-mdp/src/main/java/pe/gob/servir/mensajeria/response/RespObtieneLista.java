package pe.gob.servir.mensajeria.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespObtieneLista<T> {

	private Integer count;
	private List<T> items;

}
