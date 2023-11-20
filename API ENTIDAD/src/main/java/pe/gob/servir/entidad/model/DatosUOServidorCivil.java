package pe.gob.servir.entidad.model;

import lombok.Getter;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.io.Serializable;

@Setter
@Getter
@Entity
public class DatosUOServidorCivil implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    private Long organigramaId;
    private Long detUnidadOrganicaId;
    private Long entidadId;
    private Long flagHabilitar;
    private Long personaId;
    private Long tipoDocumentoId;
    private String nroDocumento;
    private String nombres;
    private String apellidoPaterno;
    private String apellidoMaterno;
    private String nombresApellidos;
    private String correoPrincipal;
    private String siglaUnidadOrganica;
    private String descUnidadOrganica;
    private String descTipoAsignacion;
    private String descPuesto;
    private String estadoRegistro;

}
