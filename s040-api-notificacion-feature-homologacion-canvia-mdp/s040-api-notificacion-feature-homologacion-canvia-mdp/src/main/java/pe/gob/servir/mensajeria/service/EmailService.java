package pe.gob.servir.mensajeria.service;

import java.io.IOException;

import javax.mail.MessagingException;

import pe.gob.servir.mensajeria.request.ReqBase;
import pe.gob.servir.mensajeria.request.ReqEmail;
import pe.gob.servir.mensajeria.response.RespBase;
import pe.gob.servir.mensajeria.response.RespMensaje;

public interface EmailService {
    
    RespBase<RespMensaje> enviarCorreo(ReqBase<ReqEmail> email) 
throws IOException, MessagingException;
}
