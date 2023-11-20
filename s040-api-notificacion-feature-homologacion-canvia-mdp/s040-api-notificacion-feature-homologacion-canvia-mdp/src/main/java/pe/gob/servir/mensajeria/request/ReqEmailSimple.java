package pe.gob.servir.mensajeria.request;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ReqEmailSimple {

	private String to;
	private String subject;
	private String text;
}
