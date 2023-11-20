package pe.gob.servir.entidad.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.bean.ReqSpBuscarUOXServidorCivil;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.ComboPuesto;
import pe.gob.servir.entidad.model.DatosUOServidorCivil;
import pe.gob.servir.entidad.model.ServidorRectorDTO;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespListarDatosUOXServidorCivil;
import pe.gob.servir.entidad.response.RespObtieneLista2;
import pe.gob.servir.entidad.service.GestionService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class GestionServiceImpl implements GestionService {

    @Autowired
    private GestionRepository gestionRepository;

    @Override
    public RespBase<RespObtieneLista2<ServidorRectorDTO>> listarServidoresRectores(
            Long entidadId, Long tipoOrganoId, Long unidadOrganicaSuperiorId,
            Long unidadOrganicaId, Long regimenLaboralId, Long tipoDocumentoId,
            String datosServCivil, String numeroDocumento ) {

        RespObtieneLista2<ServidorRectorDTO> response = new RespObtieneLista2<>();

        Map<String, Object> parametroMap =
                this.mapearDatosEntradaBuscarServRectores(entidadId, tipoOrganoId, unidadOrganicaSuperiorId,
                        unidadOrganicaId, regimenLaboralId, tipoDocumentoId,
                        datosServCivil, numeroDocumento);

        List<ServidorRectorDTO> lstServidoresRectores =
                this.gestionRepository.listarServidoresRectores(parametroMap);
        response.setCount(lstServidoresRectores.size());
        response.setItems(lstServidoresRectores);
        return new RespBase<RespObtieneLista2<ServidorRectorDTO>>().ok(response);
    }

    @Override
    public RespBase<RespListarDatosUOXServidorCivil> listarDatosUOxSC(Long entidadId, Long tipoDocumentoId, String nroDocumento) {

        RespBase<RespListarDatosUOXServidorCivil> respPayload = new RespBase<>();

        RespListarDatosUOXServidorCivil response = new RespListarDatosUOXServidorCivil();

        List<DatosUOServidorCivil> listaDatUOSC =
                this.gestionRepository.listarUOsByServCiv(new ReqSpBuscarUOXServidorCivil(entidadId, tipoDocumentoId, nroDocumento));


        List<RespListarDatosUOXServidorCivil.DatosUO> lstUOs = new ArrayList<>();

        if (!listaDatUOSC.isEmpty()) {

            listaDatUOSC.forEach(
                    item -> {
                        List<RespListarDatosUOXServidorCivil.DatosPuesto> lstPuestos = new ArrayList<>();

                        RespListarDatosUOXServidorCivil.DatosUO datosUO = new RespListarDatosUOXServidorCivil.DatosUO();
                        datosUO.setDetalleUOId(item.getDetUnidadOrganicaId());
                        datosUO.setOrganigramaId(item.getOrganigramaId());
                        datosUO.setSiglaUO(item.getSiglaUnidadOrganica());
                        datosUO.setDescUO(item.getDescUnidadOrganica());

                        List<ComboPuesto> listarPuestosEntidOrgId = this.gestionRepository.buscarPuestos(item.getEntidadId(), item.getOrganigramaId());
                        if (!listarPuestosEntidOrgId.isEmpty()) {
                            listarPuestosEntidOrgId.forEach(
                                    puesto -> {
                                        RespListarDatosUOXServidorCivil.DatosPuesto datPuestoDto = new RespListarDatosUOXServidorCivil.DatosPuesto();
                                        datPuestoDto.setPuestoId(puesto.getId());
                                        datPuestoDto.setDescPuesto(puesto.getDescripcion());
                                        lstPuestos.add(datPuestoDto);
                                    }
                            );
                        }
                        datosUO.setListaPuestos(lstPuestos);
                        lstUOs.add(datosUO);
                    }
            );

            response.setEntidad(listaDatUOSC.get(0).getEntidadId());
            response.setPersonaId(listaDatUOSC.get(0).getPersonaId());
            response.setNroDocumento(listaDatUOSC.get(0).getNroDocumento());
            response.setNombres(listaDatUOSC.get(0).getNombres());
            response.setApellidoPaterno(listaDatUOSC.get(0).getApellidoPaterno());
            response.setApellidoMaterno(listaDatUOSC.get(0).getApellidoMaterno());
            response.setCorreoPrincipal(listaDatUOSC.get(0).getCorreoPrincipal());
            response.setListaUO(lstUOs);
        } else {
            return ParametrosUtil.setearResponse(respPayload, Boolean.FALSE, "No hay informacion con los datos ingresados");
        }

        return new RespBase<RespListarDatosUOXServidorCivil>().ok(response);
    }

    private Map<String, Object> mapearDatosEntradaBuscarServRectores (
                    Long entidadId, Long tipoOrganoId, Long unidadOrganicaSuperiorId,
                    Long unidadOrganicaId, Long regimenLaboralId, Long tipoDocumentoId,
                    String datosServCivil, String numeroDocumento) {

        Map<String, Object> parametroMap = new HashMap<>();
        parametroMap.put(Constantes.ENTIDADID, entidadId);
        parametroMap.put(Constantes.TIPOORGANOID, tipoOrganoId);
        parametroMap.put(Constantes.UNIDADORGANICASUPERIORID, unidadOrganicaSuperiorId);
        parametroMap.put(Constantes.UNIDADORGANICAID, unidadOrganicaId);
        parametroMap.put(Constantes.REGIMENLABORALID, regimenLaboralId);
        parametroMap.put(Constantes.TIPODOCUMENTOID, tipoDocumentoId);
        parametroMap.put(Constantes.DATOSSERVCIVIL, datosServCivil);
        parametroMap.put(Constantes.NUMERODOCUMENTO, numeroDocumento);
        return parametroMap;
    }

}
