package pe.gob.servir.mensajeria.service.impl;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Base64;
import java.util.List;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.InputStreamSource;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

import pe.gob.servir.mensajeria.common.VariablesSistema;
import pe.gob.servir.mensajeria.model.PlantillaCorreo;
import pe.gob.servir.mensajeria.request.ReqBase;
import pe.gob.servir.mensajeria.request.ReqEmail;
import pe.gob.servir.mensajeria.request.ReqEmailAttachment;
import pe.gob.servir.mensajeria.response.RespBase;
import pe.gob.servir.mensajeria.response.RespMensaje;
import pe.gob.servir.mensajeria.service.EmailService;
import pe.gob.servir.mensajeria.service.PlantillaCorreoService;
import pe.gob.servir.mensajeria.util.Util;

@Service
public class EmailServiceImpl implements EmailService {

	@Autowired
	private JavaMailSender emailSender;

	@Autowired
	private PlantillaCorreoService plantillaService;

	@Autowired
	private VariablesSistema variablesSistema;

	@Override
	public RespBase<RespMensaje> enviarCorreo(ReqBase<ReqEmail> email) throws IOException, MessagingException {
		RespMensaje payload = new RespMensaje();
		String mensaje = "";
		String asunto = "";
		try {
			if (email.getPayload().getData() != null) {

				PlantillaCorreo plantilla = plantillaService.obtenerPlantilla(email.getPayload().getData().getTemplateCode());

				if (plantilla != null) {
					String existeSubject = (email.getPayload().getData().getSubject() == null) ? ""	: email.getPayload().getData().getSubject();
					
					if (existeSubject.isEmpty()) {
						asunto = plantilla.getAsuntoPlantilla();
					} else {
						asunto = email.getPayload().getData().getSubject();
					}

					String content = Util.velocityEngine(plantilla.getHtml(), email.getPayload().getData().getBodyValues());
					
					if (email.getPayload().isIncludeAttachments()) {
						if (email.getPayload().getData().getAttachments() != null && email.getPayload().getData().getAttachments().size() > 0) {
							mensaje = sendHtmlMessage(email.getPayload().getData().getTo(), asunto, content, email.getPayload().getData().getAttachments());
						} else {
							mensaje = "No se envio ningun archivo adjunto.";
						}
					} else {
						mensaje = sendHtmlMessage(email.getPayload().getData().getTo(), asunto, content);
					}
					mensaje = "Correo enviado con éxito.";
				} else {
					mensaje = "El código de plantilla es incorrecto.";
				}

			} else {
				mensaje = "No hay suficientes datos para enviar el correo.";
			}

		} catch (Exception e) {
			e.printStackTrace();
			mensaje = "Ocurrio un error al enviar el correo.";
		}
		payload.setMensaje(mensaje);
		return new RespBase<RespMensaje>().ok(payload);
	}

	private String sendHtmlMessage(String to, String subject, String htmlBody) throws MessagingException {
		return sendHtmlMessage(to, subject, htmlBody, null);
	}
	
	private String sendHtmlMessage(String to, String subject, String htmlBody, List<ReqEmailAttachment> attachments) throws MessagingException {
		String mensaje = "";
		MimeMessage message = emailSender.createMimeMessage();
		MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");
		helper.setFrom(variablesSistema.correo);
		helper.setTo(listaCorreos(to));
		helper.setSubject(subject);
		helper.setText(htmlBody, true);
		
		if (attachments != null && attachments.size() > 0) {
			try {
				for (ReqEmailAttachment attachment : attachments) {
					InputStreamSource attachFile;
						attachFile = new ByteArrayResource(Base64.getDecoder().decode(new String(attachment.getContent()).getBytes("UTF-8")));
						helper.addAttachment(attachment.getFileName(), attachFile);
				}
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				mensaje = "Error al decodificar archivo adjunto.";
			}
		}

		emailSender.send(message);
		return mensaje;
	}

	private InternetAddress[] listaCorreos(String lista) throws AddressException {
		if (lista != null) {
			String[] correos = lista.split(",");
			if (correos.length > 0) {
				InternetAddress[] address = new InternetAddress[correos.length];
				for (int i = 0; i < correos.length; i++) {
					address[i] = new InternetAddress(correos[i]);
				}
				return address;
			}
		}
		return new InternetAddress[0];
	}

}
