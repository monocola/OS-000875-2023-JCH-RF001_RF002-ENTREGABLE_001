package pe.gob.servir.entidad.request;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.request.dto.CorreoDTO;
import pe.gob.servir.entidad.request.dto.EntidadDTO;
import pe.gob.servir.entidad.request.dto.LogoDTO;
import pe.gob.servir.entidad.request.dto.TelefonoDTO;

@Getter
@Setter
public class ReqEntidad {

	@NotNull(message = "Campo entidad es obligatorio")
	@Valid
	private EntidadDTO entidad;
	@Valid
	private List<TelefonoDTO>	telefonos;
	@Valid
	private List<CorreoDTO> correos;
	@Valid
	private LogoDTO logo;
	
	@Valid
	private LogoDTO portada;
	
	@Valid
	private String razonSocial;
	
	private String ruc;
	
	private Integer actualizaRazon ;

	@Override
	public String toString() {
		return "ReqEntidad [entidad=" + entidad.toString() + ", telefonos=" + telefonos + ", correos=" + correos + ", logo=" + logo
				+ ", portada=" + portada + ", razonSocial=" + razonSocial + ", ruc=" + ruc + ", actualizaRazon="
				+ actualizaRazon + "]";
	}
}
