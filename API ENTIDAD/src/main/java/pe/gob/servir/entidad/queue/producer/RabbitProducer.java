package pe.gob.servir.entidad.queue.producer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.common.RoutingKey;
import pe.gob.servir.entidad.common.VariablesSistema;


@Component
public class RabbitProducer {

	private static final Logger logger = LoggerFactory.getLogger(RabbitProducer.class);
	@Autowired
	VariablesSistema variablesSistema;

    private static final String EXCHANGE_NAME = "api.proceso.masivo.exchange";
	private final RabbitTemplate rabbitTemplate;

	public RabbitProducer(final RabbitTemplate rabbitTemplate) {
		this.rabbitTemplate = rabbitTemplate;
	}
	 
	public void writeMessage(RoutingKey routingKey, String message) {
		try {
			logger.info("routingKey.getKey():" + routingKey.getKey());
			logger.info("message:" + message);

			if (RoutingKey.SEND_PROCESO_MASIVO.getKey().equals(routingKey.getKey())) {
				rabbitTemplate.convertAndSend(EXCHANGE_NAME, routingKey.getKey(), message);
				logger.info("EXCHANGE_NAME:" + EXCHANGE_NAME);
				logger.info("rabbitTemplate:" + rabbitTemplate.toString());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}	 
	
}