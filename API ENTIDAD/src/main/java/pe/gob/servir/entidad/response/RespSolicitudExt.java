package pe.gob.servir.entidad.response;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class RespSolicitudExt {

	
	// entidad
	private Long personaId;
	private Long entidadId;
	private String razonSocial;
    private String nombreEntidad;
    private String actividadEconomicaPrincipal;
    private String estadoContribuyente;
    private String condicionContribuyente;
    private String validadoSunat;
    private String rucEntidad;
    
	private String abreviatura;
	private Long nivelGobId;
	private String nivelGob;
	private Long sectorId;
	private String sector;
	private Long tipoEntidadId;
	private String tipoEntidad;
	private Long documentoId;
	private Integer tipoDocumento;
	private String flagEntidadExite;
	private Integer cantidadSol;
	
	private String urlLogoEntidad;
	private String fechaNacimiento;
	private String telefonoFijo;
	private String celular;
	private String correoElectronico;
	private String uuidDocumento;
	private Integer estadoSolicitud;
	private Long solicitudEntidadExtId;
	
}
