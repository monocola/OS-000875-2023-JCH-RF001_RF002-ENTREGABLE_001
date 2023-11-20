package pe.gob.servir.entidad.bean;

import java.io.Serializable;

import lombok.Data;

@Data
public class ReqSpBuscarUOXServidorCivil implements Serializable {


	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Long entidadId;
    private Long tipoDocumentoId;
    private String nroDocumento;

    public ReqSpBuscarUOXServidorCivil() {
    }

    public ReqSpBuscarUOXServidorCivil(Long entidadId, Long tipoDocumento, String nroDocumento) {
        this.entidadId = entidadId;
        this.tipoDocumentoId = tipoDocumento;
        this.nroDocumento = nroDocumento;
    }
}
