package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.model.GenericoDTO;

@Getter
@Setter
@ToString
public class RespListaServidoresCivilesGraficosDonats {
	
	private List<GenericoDTO>  servidoresCivilesTipoOrgano;
	private List<GenericoDTO>  servidoresCivilesRegimenLaboral;
	
}
