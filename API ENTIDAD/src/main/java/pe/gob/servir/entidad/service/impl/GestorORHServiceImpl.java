package pe.gob.servir.entidad.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.model.Gestores;
import pe.gob.servir.entidad.repository.GestoresRepository;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.ResponseDTO;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.GestorORHService;
import pe.gob.servir.entidad.util.ParametrosUtil;

import java.time.Instant;
import java.util.Optional;

@Service
public class GestorORHServiceImpl implements GestorORHService {

    @Autowired
    private GestoresRepository gestoresRepository;

    @Override
    public RespBase<Object> anularGestorORH(MyJsonWebToken jwt, Long gestorId) {

        RespBase<Object> response = new RespBase<>();
        Optional<Gestores> oGestores = this.gestoresRepository.findById(gestorId);

        if (oGestores.isPresent()) {
            Gestores gestores = oGestores.get();
            gestores.setCampoSegUpd("0", jwt.getUsuario().getUsuario(), Instant.now());
            this.gestoresRepository.save(gestores);
            return new RespBase<Object>().ok(new ResponseDTO(gestorId.toString(), null, Constantes.OPERACION_EXITOSA));
        } else {
            return ParametrosUtil.setearResponse(response, Boolean.FALSE, "Gestor no existe");
        }
    }

}
