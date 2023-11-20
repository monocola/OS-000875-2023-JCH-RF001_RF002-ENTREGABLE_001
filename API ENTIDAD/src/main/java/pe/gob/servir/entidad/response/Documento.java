package pe.gob.servir.entidad.response;

import lombok.Data;

import java.time.LocalDate;

@Data
public class Documento {
    private Long documentoId;
    private Long personaId;
    private Integer tipoDocumento;
    private String numeroDocumento;
    private LocalDate fechaCaducidad;
}
