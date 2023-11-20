package pe.gob.servir.entidad.api.dto;

import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DataEmail {
	private String templateCode;
	private String to;
	private String subject;
	private Map<String,Object> bodyValues;
	private List<ReqEmailAttachment> attachments;

}
