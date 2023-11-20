package pe.gob.servir.entidad.request.dto;

import java.io.Serializable;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;


@Getter
@Setter
@ToString
public class EditParticipanteGDRDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Valid
	@NotNull(message = "campo detalleUoId es obligatorio")
	private Long detUoId;
	private Long segmentoId;
	private Long rolId;
	private Long flagHabilitar;
	private Long estadoSrvCivGdrId;
	private Long personaEvaluadorId;
	
	private Long indicadorMeta;
}
