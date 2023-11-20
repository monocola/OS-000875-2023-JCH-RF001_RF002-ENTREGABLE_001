package pe.gob.servir.entidad.response;

import lombok.Data;

@Data
public class Correo {
    private Long correoId;
    private Long personaId;
    private String tipoCorreo;
    private String correo;
}
