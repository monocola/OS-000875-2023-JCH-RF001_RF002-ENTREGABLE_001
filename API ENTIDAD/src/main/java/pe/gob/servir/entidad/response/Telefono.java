package pe.gob.servir.entidad.response;

import lombok.Data;

@Data
public class Telefono {
    private Long telefonoId;
    private Long personaId;
    private String tipoTelefono;
    private String codigoArea;
    private String numeroTelefono;
    private String numeroAnexo;
}
