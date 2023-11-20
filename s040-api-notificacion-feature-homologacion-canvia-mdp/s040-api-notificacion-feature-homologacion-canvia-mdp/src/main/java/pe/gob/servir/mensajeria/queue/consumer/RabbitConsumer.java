package pe.gob.servir.mensajeria.queue.consumer;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.Queue;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.support.AmqpHeaders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rabbitmq.client.Channel;

import pe.gob.servir.mensajeria.common.RoutingKey;
import pe.gob.servir.mensajeria.common.VariablesSistema;
import pe.gob.servir.mensajeria.request.ReqBase;
import pe.gob.servir.mensajeria.request.ReqEmail;
import pe.gob.servir.mensajeria.service.EmailService;

@Component
public class RabbitConsumer {

	private static final Logger logger = LoggerFactory.getLogger(RabbitConsumer.class);
	private static final String QUEUE_NAME = "email.masivo.queue";
	private static final String QUEUE_NAME_CREDENCIALES = "email.credenciales.queue";
	private ObjectMapper mapper = new ObjectMapper();

	
	@Autowired
	EmailService emailService;
	
	@Autowired
	VariablesSistema variablesSistema;
	
	public RabbitConsumer() {
		super();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	}
	
	@Bean
	public Queue queue() {
		//Se modifico la propiedad durable y se agrega argumento x-dead-letter-exchange 	
		Map<String, Object> args_masivo = new HashMap<>();
		args_masivo.put("x-dead-letter-exchange", "api.email.masivo.dl.exchange");
		return new Queue(QUEUE_NAME, true, false, false ,args_masivo); 

//		return new Queue(QUEUE_NAME, true);		
	}
	
	@Bean
	public Queue queueCredenciales() {
		
		//Se modifico la propiedad durable y se agrega argumento x-dead-letter-exchange 	
		Map<String, Object> args_credencial = new HashMap<>();
		args_credencial.put("x-dead-letter-exchange", "api.email.credenciales.dl.exchange");
		return new Queue(QUEUE_NAME_CREDENCIALES, true, false, false ,args_credencial); 

//		return new Queue(QUEUE_NAME_CREDENCIALES, true);
	}
	
	@RabbitListener(queues = QUEUE_NAME, ackMode = "MANUAL")
	public void listener(Message message, Channel channel, @Header(AmqpHeaders.DELIVERY_TAG) long deliveryTag,
			@Header(AmqpHeaders.RECEIVED_ROUTING_KEY) String routingKey) throws IOException {
		try {
			logger.info("Iniciando RabbitConsumer");			
			logger.info("{}",channel.toString());
			String body = new String(message.getBody(), "UTF-8");
			if (RoutingKey.SEND_EMAIL_MASIVO.getKey().equals(routingKey)) {
				logger.info("message.body={} de queue email.masivo.queue", body);
				JavaType type = mapper.getTypeFactory().constructParametricType(ReqBase.class, ReqEmail.class);
				ReqBase<ReqEmail> request = mapper.readValue(body, type);				
				emailService.enviarCorreo(request);
				channel.basicAck(deliveryTag, false);
				channel.close();
			}else {
				logger.warn("routingKey={} no implementado", routingKey);
				channel.basicNack(deliveryTag, false, false);
			}
			logger.info("finalizando RabbitConsumer");				
		} catch (Exception e) {
			channel.basicNack(deliveryTag, false, false);
		}

	}
	
	@RabbitListener(queues = QUEUE_NAME_CREDENCIALES, ackMode = "MANUAL")
	public void listenerCrendenciales(Message message, Channel channel, @Header(AmqpHeaders.DELIVERY_TAG) long deliveryTag,
			@Header(AmqpHeaders.RECEIVED_ROUTING_KEY) String routingKey) throws IOException {
		try {
			logger.info("Iniciando RabbitConsumer");			
			logger.info("{}",channel.toString());
			String body = new String(message.getBody(), "UTF-8");
			if (RoutingKey.SEND_EMAIL_CREDENCIALES.getKey().equals(routingKey)) {
				logger.info("message.body={} de email.credenciales.queue", body);
				JavaType type = mapper.getTypeFactory().constructParametricType(ReqBase.class, ReqEmail.class);
				ReqBase<ReqEmail> request = mapper.readValue(body, type);				
				emailService.enviarCorreo(request);
				channel.basicAck(deliveryTag, false);
				channel.close();
			}
			else {
				logger.warn("routingKey={} no implementado", routingKey);
				channel.basicNack(deliveryTag, false, false);
			}
			logger.info("finalizando RabbitConsumer");				
		} catch (Exception e) {
			channel.basicNack(deliveryTag, false, false);
		}

	}
	
	

}
