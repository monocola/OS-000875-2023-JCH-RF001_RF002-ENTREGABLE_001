package pe.gob.servir.entidad.response;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Data;

@Data
public class Direccion {
    private Long direccionId;
    private Long personaId;
    private Long ubigeoId;
    private transient JsonNode ubigeoFull;
    private String tipoDireccion;
    private String direccionCompleta;
    private String referencia;
}
