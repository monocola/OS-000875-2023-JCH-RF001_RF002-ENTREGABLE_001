package pe.gob.servir.entidad.api.dto;

import lombok.Data;

import java.io.Serializable;

@Data
public class RespRespSubirArchivoAlfresco implements Serializable {

    private Long codigoRespuesta;
    private String mensajeRespuesta;
    private String uuid;

}
