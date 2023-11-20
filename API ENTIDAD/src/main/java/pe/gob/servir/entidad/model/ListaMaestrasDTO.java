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
public class ListaMaestrasDTO { 
	@Id	
	private Integer parametroId;
	private String tipoParametro;
	private String codigoTexto;
	private Integer codigoNumero;
	private String valorTexto;
	private Integer valorNumero;
	private Long valorFecha;
	
	private String descripcion;
	private String estadoRegistro;
    private String usuarioCreacion;
    private Date fechaCreacion;
    private String usuarioModificacion;
    private Date fechaModificacion;
    
    
    
    
    


}
