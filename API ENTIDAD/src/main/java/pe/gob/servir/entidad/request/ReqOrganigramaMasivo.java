package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.request.dto.OrganoExcelDTO;

@Getter
@Setter
public class ReqOrganigramaMasivo {
	
	@NotNull(message = "Campo listaOrgano es obligatorio")
	@Valid
	private List<OrganoExcelDTO> listaOrgano;

}
