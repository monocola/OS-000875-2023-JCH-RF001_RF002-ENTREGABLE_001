package pe.gob.servir.entidad.model;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Getter
@Setter
public class ObtenerSolicitud {
	@Id
	private Integer idSolicitudEntidad;
	private String razonSocial;
//	private String rucId;
	private String ruc;
	private Integer sectorId;
	private String sector;
	private Integer nivelGobiernoId;
	private String nivelGobierno;
	private Date fechaRegistro;
	private String tipoDocumentoId;
	private String descripcionTipoDocumento;
	private String numeroDocumento;
	private String nombres;
	private String apellidoPaterno;
	private String apellidoMaterno;
	private String correLaboral;
	private String correoOpcional;
	private String telefono;
	private String celular;
	private String celularAlterno;
	private Integer puestoId;
	private String descripcionPuesto;
	private Integer distritoId;
	private String distrito;
	private Integer provinciaId;
	private String provincia;
	private Integer departamentoId;
	private String departamento;
	private Integer estadoId;
	private String estadoSolicitud;
	private String fechaAlta;
	private String fechaBaja;
	private Integer cargoId;
	private String descripcionCargo;
}
