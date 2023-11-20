package pe.gob.servir.entidad.request;

import javax.validation.Valid;
import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.model.AuditEntity;

@Getter
@Setter
public class ReqListarPuesto extends AuditEntity {
//	@NotNull(message = "Campo cuentaEntidad es obligatorio")
	@Valid
	private static final long serialVersionUID = 1L;
	private String esJefe;
	private Long unidadOrganicaID;
	private String nombrePuesto;	
}



