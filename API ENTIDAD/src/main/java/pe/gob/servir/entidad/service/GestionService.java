package pe.gob.servir.entidad.service;

import pe.gob.servir.entidad.model.ServidorRectorDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespListarDatosUOXServidorCivil;
import pe.gob.servir.entidad.response.RespObtieneLista2;

public interface GestionService {

    RespBase<RespObtieneLista2<ServidorRectorDTO>> listarServidoresRectores (Long entidadId, Long tipoOrganoId, Long unidadOrganicaSuperiorId,
                                                                             Long unidadOrganicaId, Long regimenLaboralId, Long tipoDocumentoId,
                                                                             String datosServCivil, String numeroDocumento);

    RespBase<RespListarDatosUOXServidorCivil> listarDatosUOxSC (Long entidadId, Long tipoDocumentoId, String nroDocumento);

}
