package pe.gob.servir.entidad.request;

import lombok.Data;
import lombok.ToString;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Data
@ToString
public class ReqRegistrarProcesoAsincrono implements Serializable {

	private static final long serialVersionUID = 1L;

	@NotNull(message = "Campo tipo proceso es obligatorio")
    @Valid
    private Long tipoProcesoId;

	@NotNull(message = "Campo usuario envio es obligatorio")
    @Valid
    private String usuarioEnvio;
	
    @NotNull(message = "Campo fecha envio es obligatorio")
    @Valid
    private String fechaEnvio;
    
    private String base64File;
    
    private String nombreArchivo;
    
    private String extensionArchivo;
    
    private String jsonVariablesInput;
}