package pe.gob.servir.entidad.model;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table
@Setter
@Getter
public class ServidorRectorDTO implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Id
    private Long detUnidadOrganicaId;
    private Integer flagHabilitar;
    private Long organigramaId;
    private Long personaId;
    private String docEntidadId;
    private String nombresApellidos;
    private String nombres;
    private String apellidoPaterno;
    private String apellidoMaterno;
    private String unidadOrganica;
    private String puesto;
    private String tipoAsignacion;
    private Integer segmentoId;
    private Long rolId;
    private String estado;
    
}
