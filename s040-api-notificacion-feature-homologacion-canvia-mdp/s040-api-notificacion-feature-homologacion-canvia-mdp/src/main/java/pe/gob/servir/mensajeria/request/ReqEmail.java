package pe.gob.servir.mensajeria.request;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ReqEmail {

	private String id;
	private ReqDataEmail data;
	private boolean includeAttachments;
}
