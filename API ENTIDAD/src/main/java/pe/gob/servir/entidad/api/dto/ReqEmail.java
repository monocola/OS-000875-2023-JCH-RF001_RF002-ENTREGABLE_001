package pe.gob.servir.entidad.api.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ReqEmail {
	private String id;
	private DataEmail data;
	private boolean includeAttachments;

}
