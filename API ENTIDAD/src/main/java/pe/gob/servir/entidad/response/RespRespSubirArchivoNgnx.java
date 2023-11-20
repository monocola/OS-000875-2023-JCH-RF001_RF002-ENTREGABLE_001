package pe.gob.servir.entidad.response;

import lombok.Data;

import java.io.Serializable;

@Data
public class RespRespSubirArchivoNgnx  implements Serializable {
    private Long codigoRespuesta;
    private String mensajeRespuesta;
    private String uuid;
    private String rutaArchivo;

}
