package pe.gob.servir.entidad.response;

import lombok.Data;

import java.util.List;

@Data
public class RespObtieneLista2<T> {
    private Integer count;
    private List<T> items;
}
