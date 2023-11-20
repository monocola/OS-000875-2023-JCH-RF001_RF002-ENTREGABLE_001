package pe.gob.servir.entidad.response;

import lombok.Data;

import java.time.LocalDate;
@Data
public class PersonaJuridica {
    private Long personaId;
    private String razonSocial;
    private String nombreComercial;
    private LocalDate fechaInscripcion;
    private LocalDate fechaInicioActividad;
    private LocalDate fechaBaja;
    private String actividadEconomicaPrincipal;
    private String estadoContribuyente;
    private String condicionContribuyente;
    private String validadoSunat;
}
