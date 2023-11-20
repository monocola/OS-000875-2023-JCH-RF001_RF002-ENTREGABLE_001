package pe.gob.servir.entidad.response;

import lombok.Data;

@Data
public class Web {
    private Long webId;
    private Long personaId;
    private String urlWeb;
}
