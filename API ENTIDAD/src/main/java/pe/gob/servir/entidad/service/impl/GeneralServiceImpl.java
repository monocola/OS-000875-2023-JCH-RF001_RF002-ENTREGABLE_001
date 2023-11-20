package pe.gob.servir.entidad.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.model.PaisesDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespPaises;
import pe.gob.servir.entidad.response.RespParametro;
import pe.gob.servir.entidad.service.GeneralService;

@Service
public class GeneralServiceImpl implements GeneralService {
	
	@Autowired
	private GeneralRepository generalRepository;
	
	@Autowired
	VariablesSistema variablesSistema;

	@Override
	public RespBase<RespParametro> comboEstado() {		
		List<Parametro> lista = generalRepository.buscarListaParametro(null, Constantes.ESTADO_REGISTRO, null);
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(lista);
		return new RespBase<RespParametro>().ok(respPayload);
	}

	@Override
	public RespBase<RespParametro> comboNivel(String tipoOrganigrama) {
		List<Parametro> lista = generalRepository.buscarListaParametro(null, Constantes.NIVEL_ORGANO, null);
		Parametro nivel;
		List<Parametro> listaFinal = new ArrayList<>();
		for (Parametro parametro : lista) {
			String strTipoOrgano = String.valueOf(variablesSistema.tipoOrgano);
			if(strTipoOrgano.equals(tipoOrganigrama)) {
				if(parametro.getValorTexto().equals(Constantes.NIVEL_1) || parametro.getValorTexto().equals(Constantes.NIVEL_2)) {
					nivel = new Parametro();
					nivel.setParametroId(parametro.getParametroId());
					nivel.setCodigoTexto(parametro.getCodigoTexto());
					nivel.setCodigoNumero(parametro.getCodigoNumero());
					nivel.setValorTexto(parametro.getValorTexto());
					nivel.setOrden(parametro.getOrden());
					nivel.setTipoParametro(parametro.getTipoParametro());
					listaFinal.add(nivel);
				}
			}else {
				if(parametro.getValorTexto().equals(Constantes.NIVEL_3)) {
					nivel = new Parametro();
					nivel.setParametroId(parametro.getParametroId());
					nivel.setCodigoTexto(parametro.getCodigoTexto());
					nivel.setCodigoNumero(parametro.getCodigoNumero());
					nivel.setValorTexto(parametro.getValorTexto());
					nivel.setOrden(parametro.getOrden());
					nivel.setTipoParametro(parametro.getTipoParametro());
					listaFinal.add(nivel);
				}
			}
		}
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(listaFinal);
		return new RespBase<RespParametro>().ok(respPayload);
	}

	@Override
	public RespBase<RespParametro> comboNaturalezaOrgano() {
		List<Parametro> lista = generalRepository.buscarListaParametro(null, Constantes.TIPO_NATURALEZA, null);
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(lista);
		return new RespBase<RespParametro>().ok(respPayload);
	}
	
	@Override
	public RespBase<RespParametro> comboTipoDocumento() {
		List<Parametro> lista = generalRepository.buscarListaParametro(null, Constantes.PARAMETRO_TIPO_DOC, null);
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(lista);
		return new RespBase<RespParametro>().ok(respPayload);
	}

	@Override
	public RespBase<RespPaises> comboPaises() {
		List<PaisesDTO> lista = generalRepository.buscarPaises();
		RespPaises respPayload = new RespPaises();
		respPayload.setListaPaises(lista);;
		return new RespBase<RespPaises>().ok(respPayload);
	}

	@Override
	public RespBase<RespParametro> comboOrgano() {
		List<Parametro> lista = generalRepository.buscarListaParametro(null, Constantes.PARAMETRO_TIPO_ORG, null);
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(lista);
		return new RespBase<RespParametro>().ok(respPayload);
	}

	@Override
	public RespBase<RespParametro> comboSexo() {
		List<Parametro> lista = generalRepository.buscarListaParametro(null, Constantes.PARAMETRO_SEXO, null);
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(lista);
		return new RespBase<RespParametro>().ok(respPayload);
	}

	@Override
	public RespBase<RespParametro> comboParametro(Integer parametroId, String tipoParametro, String codigoTexto){
		List<Parametro> lista = generalRepository.buscarListaParametro(parametroId, tipoParametro, codigoTexto);
		RespParametro respPayload = new RespParametro();
		respPayload.setListaParametros(lista);		
		return new RespBase<RespParametro>().ok(respPayload);
	}
}
