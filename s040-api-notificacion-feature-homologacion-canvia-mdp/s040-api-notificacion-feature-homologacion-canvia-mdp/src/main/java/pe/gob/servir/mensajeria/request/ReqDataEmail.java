package pe.gob.servir.mensajeria.request;

import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ReqDataEmail {
	private String templateCode;
	private String to;
	private String subject;
	private Map<String,Object> bodyValues;
	private List<ReqEmailAttachment> attachments;
}
