package pe.gob.servir.entidad.response;

import lombok.Data;

import java.io.Serializable;

@Data
public class RespRegistrarSolicitudExterna implements Serializable {

    private Long codigoRespuesta;
    private String mensajeRespuesta;
    private Long solicitudEntidadExtId;
    private String rutaArchivo;

}
