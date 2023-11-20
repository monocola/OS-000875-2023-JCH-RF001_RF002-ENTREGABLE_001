package pe.gob.servir.entidad.bean;

import lombok.Data;

import java.io.Serializable;

@Data
public class ValidaExtensiones implements Serializable {

    private static final long serialVersionUID = 1L;
    private Boolean validacionExtensiones;
    private String extensiones;

}
