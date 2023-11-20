package pe.gob.servir.entidad.response;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
public class RespListarDatosUOXServidorCivil implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long entidad;
    private Long personaId;
    private String nroDocumento;
    private String nombres;
    private String apellidoPaterno;
    private String apellidoMaterno;
    private String correoPrincipal;
    private List<DatosUO> listaUO;

    @Data
    public static class DatosUO implements Serializable{
        private static final long serialVersionUID = 1L;
        private Long detalleUOId;
        private Long organigramaId;
        private String siglaUO;
        private String descUO;
        private List<DatosPuesto> listaPuestos;
    }

    @Data
    public static class DatosPuesto implements Serializable{
        private static final long serialVersionUID = 1L;
        private Long puestoId;
        private String descPuesto;
    }


}
