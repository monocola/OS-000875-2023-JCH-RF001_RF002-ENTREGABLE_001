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
public class ListaEntidades { 
	@Id	
	private Long nroCuentas;
	private Long cuentaId;	
	private String nombres;
	private String apellidoMaterno;
	private Integer usuarioId;
	private Integer personaId;
	private String apellidoPaterno;
	private String descripcionPuesto;
	private Integer documentoId;
	private Integer tipoDocumento;
	private String descTipoDocumento;
	private String nroDocumento;
	private String estadoId;
	private String descripcionEstado;
	private Integer paisId;
	private String nombrePais;
	private Integer correoId;
	private String anexo;
	private String fechaAlta;
	private String flagCuentaEditable;
	private String correo;
	private String numeroTelefono;
	private Integer telefonoId;
	private String fechaBaja;


}
