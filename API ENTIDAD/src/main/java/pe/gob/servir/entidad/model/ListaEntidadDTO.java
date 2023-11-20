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
public class ListaEntidadDTO {
	@Id	
	private Integer entidadId;
	private Integer sectorId;
	private String sector;
	private Integer nivelGobiernoId;
	private String nivelGobierno;
	private String descripcionEntidad;
	private String sigla;
	private Integer personaId;
	private String razonSocial;
	private String nombreComercial;
	private Integer tipoDocumento;
	private String numeroDocumento;
	private Integer direccionId;
	private String direccion;
	private Integer correoId;
	private String correo;
	private Integer telefonoId;
	private String telefono;
	private String anexo;
	private String logo;
	private String urlWeb;
	private Integer distritoId;
	private String distrito;
	private Integer provinciaId;
	private String provincia;
	private Integer departamentoId;
	private String departamento;
	private String flagActualiza;
	private Integer direccionFiscalId;
	private String direccionFiscal;
	private String urlPortada;
	private String nroSindicatos;
	private String  tipoEntidadPub;
	private Integer tipoEntidadPubId;
}
