package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import pe.gob.servir.entidad.model.GenericoDTO;

@Getter
@Setter
@ToString
public class RespListaServidoresCivilesGDRGraficosDonats {
	
	private List<GenericoDTO>  servidoresCivilesTipoOrganoGdr;
	private List<GenericoDTO>  servidoresCivilesRegimenLaboralGdr;
	private List<GenericoDTO>  servidoresCivilesCarrerasEspecialesGdr;
	private List<GenericoDTO>  servidoresCivilesSindicalizadosGdr;
	private List<GenericoDTO>  servidoresCivilesPorSegmentoGdr;
	
}
