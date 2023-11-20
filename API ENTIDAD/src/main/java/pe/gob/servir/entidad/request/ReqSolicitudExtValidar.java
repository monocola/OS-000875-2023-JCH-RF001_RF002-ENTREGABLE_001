package pe.gob.servir.entidad.request;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

public class ReqSolicitudExtValidar {

	@NotNull(message = "Campo rucEntidad es obligatorio")
    @Valid
    private String rucEntidad;

    @NotNull(message = "Campo razonSocial es obligatorio")
    @Valid
    private String razonSocial;

    @NotNull(message = "Campo abreviatura es obligatorio")
    @Valid
    private String abreviatura;
    private String nombreEntidad;
    private Long nivelGobiernoId;
    private String nivelGobierno;
    private Long sectorId;
    private String sector;
    private Long tipoEntidadId;
    private String tipoEntidad;

    @NotNull(message = "Campo tipoDocumento es obligatorio")
    @Valid
    private Integer tipoDocumento;

    @NotNull(message = "Campo numeroDocumento es obligatorio")
    @Valid
    private String numeroDocumento;

    @NotNull(message = "Campo apellidoPaterno es obligatorio")
    @Valid
    private String apellidoPaterno;

    @NotNull(message = "Campo apellidoMaterno es obligatorio")
    @Valid
    private String apellidoMaterno;

    @NotNull(message = "Campo nombres es obligatorio")
    @Valid
    private String nombres;

    @NotNull(message = "Campo fechaNacimiento es obligatorio")
    @Valid
    private String fechaNacimiento;

    @NotNull(message = "Campo telefonoFijo es obligatorio")
    @Valid
    private String telefonoFijo;
    private String anexo;
    private String celular;

    @NotNull(message = "Campo correoElectronico es obligatorio")
    @Valid
    private String correoElectronico;

    @NotNull(message = "Campo estadoSolicitud es obligatorio")
    @Valid
    private Integer estadoSolicitud;

    @NotNull(message = "Campo uuId es obligatorio")
    @Valid
    private String uuId;

    private String base64Image;
    
}
