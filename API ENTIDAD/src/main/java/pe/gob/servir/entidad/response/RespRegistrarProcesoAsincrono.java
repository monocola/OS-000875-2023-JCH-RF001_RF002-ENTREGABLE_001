package pe.gob.servir.entidad.response;

import lombok.Data;

import java.io.Serializable;

@Data
public class RespRegistrarProcesoAsincrono implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long codigoRespuesta;
    private String mensajeRespuesta;
    private Long procesoId;

}