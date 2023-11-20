package pe.gob.servir.mensajeria.common;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;
@Component
@Getter
@Setter
public class VariablesSistema {
	
	@Value("${spring.mail.username}")
	public String correo;
	
	@Value("${spring.rabbitmq.addresses}")
	public String rabbitAddress;
}
