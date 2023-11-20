package pe.gob.servir.entidad.response;

import lombok.Data;

@Data
public class Persona {
    private Long personaId;
    private String tipoPersona;
    private Long documentoId;
    private Long direccionId;
    private Long telefonoId;
    private Long correoId;
    private Long webId;
    private String estadoPersona;
}
