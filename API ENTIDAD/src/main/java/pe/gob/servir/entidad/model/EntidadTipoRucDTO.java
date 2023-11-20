package pe.gob.servir.entidad.model;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class EntidadTipoRucDTO {

	@Id
	private Long personaId;
	private Long entidadId;
	private String razonSocial;
    private String nombreEntidad;
    private String abreviatura;
    private Integer tipoDocumento;
    private String rucEntidad;
    private Long documentoId;
	private Long nivelGobId;
	private String nivelGob;
	private Long sectorId;
	private String sector;
	private Long tipoEntidadId;
	private String tipoEntidad;
	private String urlLogoEntidad;
	
}
