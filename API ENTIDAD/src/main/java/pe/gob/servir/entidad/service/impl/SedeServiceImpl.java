package pe.gob.servir.entidad.service.impl;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import pe.gob.servir.entidad.common.EstadoRegistro;
import pe.gob.servir.entidad.model.Direccion;
import pe.gob.servir.entidad.model.ListaSedeDTO;
import pe.gob.servir.entidad.model.Sede;
import pe.gob.servir.entidad.repository.DireccionRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.SedeRepository;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqSede;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerSede;
import pe.gob.servir.entidad.response.RespSede;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.SedeService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class SedeServiceImpl implements SedeService{
	
	@Autowired
	private GestionRepository gestionRepository;

	@Autowired
	private SedeRepository sedeRepository;
	
	@Autowired
	private DireccionRepository direccionRepository;
	
	
	@Override
	public RespBase<RespObtenerSede> buscarSedeByFilter(Map<String, Object> parametroMap) {
		List<ListaSedeDTO> lista = gestionRepository.buscarSedeByFilter(parametroMap);
		RespObtenerSede respPayload = new RespObtenerSede();
		respPayload.setListarSede(lista);
		return new RespBase<RespObtenerSede>().ok(respPayload);
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<RespSede> guardarSede(ReqBase<ReqSede> request, MyJsonWebToken token, Long sedeId) {
		RespBase<RespSede> response = new RespBase<>();
		Sede sede = null;
		Direccion direccion = null;
		if(sedeId!=null) {
			Optional<Sede> sedeFind = sedeRepository.findById(sedeId);
			if (sedeFind.isPresent()) {
				sede = sedeFind.get();
				Optional<Direccion> direccionFind = direccionRepository.findById(sede.getDireccionId());
				if(direccionFind.isPresent()) {
					direccion = direccionFind.get();
					direccion.setUbigeoId(request.getPayload().getUbigeo());
					direccion.setDireccion(request.getPayload().getDireccion());
					direccion.setCampoSegUpd(request.getPayload().getEstadoRegistro(), token.getUsuario().getUsuario(),
							Instant.now());
					direccionRepository.save(direccion);
					sede.setDireccionId(direccion.getDireccionId());
					sede.setDireccion(direccion);
					sede.setTelefono(request.getPayload().getTelefono());
					sede.setAnexo(request.getPayload().getAnexo());
					sede.setNombreRepresentante(request.getPayload().getRepresentante());
				}else {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
							"No Existe el direccionId Ingresado");
					return response;
				}
				
				
				sede.setCampoSegUpd(request.getPayload().getEstadoRegistro(), token.getUsuario().getUsuario(),
						Instant.now());
			}else {
				response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
						"No Existe el sedeId Ingresado");
				return response;
			}
			
		}else {
			
			direccion = new Direccion();
			direccion.setUbigeoId(request.getPayload().getUbigeo());
			direccion.setDireccion(request.getPayload().getDireccion());
			direccion.setZonaGeografica(null);
			direccion.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			direccionRepository.save(direccion);
			
			sede = new Sede();
			sede.setCampoSegIns(token.getUsuario().getUsuario(), Instant.now());
			sede.setDireccionId(direccion.getDireccionId());
			sede.setDireccion(direccion);
		}
		
		sede.setEntidadId(request.getPayload().getEntidadId());
		sede.setNombreSede(request.getPayload().getNombreSede());
		sede.setSedePadreId(request.getPayload().getPadreSedeId());
		sede.setAmbito(request.getPayload().getAmbito());
		sede.setTelefono(request.getPayload().getTelefono());
		sede.setAnexo(request.getPayload().getAnexo());
		sede.setNombreRepresentante(request.getPayload().getRepresentante());
		sedeRepository.save(sede);
		
		RespSede payload = new RespSede();
		payload.setSede(sede);
		return new RespBase<RespSede>().ok(payload);
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> eliminarSede(MyJsonWebToken token, Long sedeId, String estado) {
		RespBase<Object> response = new RespBase<>();
		Optional<Sede> sedeFiind =  sedeRepository.findById(sedeId);
		if (sedeFiind.isPresent()){
			Sede sede = sedeFiind.get();
			if(estado.equals(EstadoRegistro.INACTIVO.getCodigo())) {
				Sede sedeFilter = new Sede();
				sedeFilter.setSedePadreId(sede.getSedeId());
				sedeFilter.setEstadoRegistro(EstadoRegistro.ACTIVO.getCodigo());
				Example<Sede> example = Example.of(sedeFilter);
				List<Sede> ltaSedeFilter = sedeRepository.findAll(example);
				if(!ltaSedeFilter.isEmpty()) {
					response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "No se puede eliminar, aún tiene sedes dependientes");
					return response; 
				}
			}
			
			sede.setEstadoRegistro(estado);
			sede.setCampoSegUpd(EstadoRegistro.INACTIVO.getCodigo(), token.getUsuario().getUsuario(),
					Instant.now());
			sedeRepository.save(sede);
			return  new RespBase<Object>().ok(sede);
		}else {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE, "No Existe el sedeId Ingresado");
		}
		return response;
	}

}
