package pe.gob.servir.entidad.response;

import lombok.Data;

import java.time.LocalDate;

@Data
public class PersonaNatural {
    private Long personaId;
    private String nombres;
    private String apellidoPaterno;
    private String apellidoMaterno;
    private String apellidoCasada;
    private String sexo;
    private String estadoCivil;
    private LocalDate fechaNacimiento;
    private LocalDate fechaFallecimiento;
    private String restriccionReniec;
    private String validadoReniec;
    private String validadoMigracion;
    private Long paisId;
}
