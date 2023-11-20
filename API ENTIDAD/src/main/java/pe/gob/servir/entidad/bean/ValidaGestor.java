package pe.gob.servir.entidad.bean;

import lombok.Data;
import pe.gob.servir.entidad.response.RespApiPersona;

import java.io.Serializable;

@Data
public class ValidaGestor implements Serializable {

    private Long codigoRespuesta;
    private String mensajeRespuesta;
    private RespApiPersona persona;

    public ValidaGestor() {}

    public ValidaGestor(Long codigoRespuesta, String mensajeRespuesta) {
        this.codigoRespuesta = codigoRespuesta;
        this.mensajeRespuesta = mensajeRespuesta;
    }


}
