package pe.gob.servir.entidad.feign.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;
import pe.gob.servir.entidad.api.dto.RespRespSubirArchivoAlfresco;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.response.RespBase;

import javax.validation.Valid;
import java.util.Map;

@FeignClient(name = "planificacionApi", url = "${jboss.private.base.url.planificacion}")
public interface PlanificacionApiClient {

    @PostMapping(path = { Constantes.ENDPOINT_UPLOAD_FILE_ALFRESCO },
            consumes = { MediaType.MULTIPART_FORM_DATA_VALUE },
            produces = { MediaType.APPLICATION_JSON_VALUE })
    RespBase<RespRespSubirArchivoAlfresco> subirArchivoAlfresco(@RequestParam(value = "archivo") MultipartFile archivo,
                                                                @Valid @RequestParam Map<String, String> request);

}
