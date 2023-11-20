package pe.gob.servir.entidad.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import feign.FeignException;
import pe.gob.servir.entidad.adapter.BeanAdapterPuesto;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.exception.ValidationException;
import pe.gob.servir.entidad.feign.client.MaestraApiClient;
import pe.gob.servir.entidad.model.ComboPuesto;
import pe.gob.servir.entidad.model.ComboUnidadOrganica;
import pe.gob.servir.entidad.model.DetUnidadOrganica;
import pe.gob.servir.entidad.model.ListaPuestoDTO;
import pe.gob.servir.entidad.model.Organigrama;
import pe.gob.servir.entidad.model.Puesto;
import pe.gob.servir.entidad.model.PuestoDatosDTO;
import pe.gob.servir.entidad.model.PuestoUOxEvaluador;
import pe.gob.servir.entidad.repository.GeneralRepository;
import pe.gob.servir.entidad.repository.GestionRepository;
import pe.gob.servir.entidad.repository.OrganigramaRepository;
import pe.gob.servir.entidad.repository.PuestoDetalleUORepository;
import pe.gob.servir.entidad.repository.PuestoRepository;
import pe.gob.servir.entidad.repository.PuestoRepository2;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqRegistrarPuesto;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespComboPuesto;
import pe.gob.servir.entidad.response.RespComboUnidadOrganica;
import pe.gob.servir.entidad.response.RespListarDetalleUO;
import pe.gob.servir.entidad.response.RespListarPuesto;
import pe.gob.servir.entidad.response.RespObtieneLista;
import pe.gob.servir.entidad.response.RespPuesto;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.OrganigramaService;
import pe.gob.servir.entidad.service.PuestoService;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Service
public class PuestoServiceImpl implements PuestoService {

	private static final Logger LOGGER = Logger.getLogger(PuestoServiceImpl.class);

	@Autowired
	private VariablesSistema variablesSistema;

	@Autowired
	private BeanAdapterPuesto adapterPuesto;

	@Autowired
	private GestionRepository gestionRepository;

	@Autowired
	GeneralRepository generalRepository;

	@Autowired
	private PuestoRepository puestoRepository;

	@Autowired
	private PuestoRepository2 puestoRepository2;

	@Autowired
	private PuestoDetalleUORepository puestoDetalleUORepository;

	@Autowired
	OrganigramaRepository organigramaRepository;

	@Autowired
	private OrganigramaService organigramaService;

	@Autowired
	private MaestraApiClient maestraApiClient;

	@Override
	public RespBase<RespComboPuesto> buscarPuestoPorEntidad(Long entidadId, Long organigramaId) {
		LOGGER.info("Metodo buscarPuestoPorEntidad...");
		List<ComboPuesto> listaPuestoFilter = gestionRepository.buscarPuestos(entidadId, organigramaId);
		RespComboPuesto respPayload = new RespComboPuesto();
		respPayload.setListaComboPuesto(listaPuestoFilter);
		return new RespBase<RespComboPuesto>().ok(respPayload);
	}

	@Override
	public RespBase<RespComboPuesto> filtrarPuestosPorDescripcion(Map<String, Object> parametroMap) {
		LOGGER.info("Metodo filtrarPuestosPorDescripcion...");
		List<ComboPuesto> listaPuestoFilter = gestionRepository.filtrarPuestos(parametroMap);
		RespComboPuesto respPayload = new RespComboPuesto();
		respPayload.setListaComboPuesto(listaPuestoFilter);
		return new RespBase<RespComboPuesto>().ok(respPayload);
	}

	@Override
	public RespBase<RespPuesto> registrarPuesto(ReqBase<ReqRegistrarPuesto> request, MyJsonWebToken jwt) {
		RespBase<RespPuesto> response = new RespBase<>();
		Puesto puesto = new Puesto();
		try {
			Optional<Puesto> valPuesto = puestoRepository.findByEntidadIdAndDescripcionAndOrganigramaIdAndEstadoRegistro(
					request.getPayload().getEntidadId(), request.getPayload().getDescripcion().toUpperCase(),
					request.getPayload().getOrganigramaId(), Constantes.ESTADO_ACTIVO);

			if (!valPuesto.isPresent()) {
				puesto.setCampoSegIns(jwt.getUsuario().getUsuario(), Instant.now());

				puesto.setEntidadId(request.getPayload().getEntidadId());
				puesto.setDescripcion(request.getPayload().getDescripcion().toUpperCase());
				puesto.setOrganigramaId(request.getPayload().getOrganigramaId());
				puesto.setEsJefe(request.getPayload().getEsJefe());

				puesto = puestoRepository.save(puesto);

				RespPuesto payload = new RespPuesto();
				payload.setPuesto(puesto);

				return new RespBase<RespPuesto>().ok(payload);
			} else {
				return ParametrosUtil.setearResponse(response, Boolean.FALSE,
						"Ya existe un puesto registrado en la UO con la misma descripción");
			}

		} catch (Exception e) {
			LOGGER.error(Constantes.MENSAJE_ERROR_REGISTRO_PUESTO + " " + e.getMessage());
			return ParametrosUtil.setearResponse(response, Boolean.FALSE, Constantes.MENSAJE_ERROR_REGISTRO_PUESTO);
		}
	}

	@Transactional(transactionManager = "entidadTransactionManager")
	@Override
	public RespBase<Object> editarPuesto(ReqBase<ReqRegistrarPuesto> request, MyJsonWebToken jwt, Long puestoId) {// NOSONAR
		RespBase<Object> response = new RespBase<>();
		Puesto puesto = null;
		try {
			if (puestoId != null) {
				Optional<Puesto> puestoFind = puestoRepository.findById(puestoId);
				if (puestoFind.isPresent()) {
					
					Optional<Puesto> valPuesto = puestoRepository.findByEntidadIdAndDescripcionAndOrganigramaIdAndEstadoRegistro(
							request.getPayload().getEntidadId(), request.getPayload().getDescripcion().toUpperCase(),
							request.getPayload().getOrganigramaId(), Constantes.ESTADO_ACTIVO);

					if (!valPuesto.isPresent()) {
						//validamos duplicidad
						puesto = puestoFind.get();
						puesto.setCampoSegUpd(Constantes.ESTADO_ACTIVO, jwt.getUsuario().getUsuario(),
								Instant.now());
						
						puesto.setEntidadId(request.getPayload().getEntidadId());
						puesto.setDescripcion(request.getPayload().getDescripcion().toUpperCase());
						puesto.setEsJefe(request.getPayload().getEsJefe());
						puesto.setOrganigramaId(request.getPayload().getOrganigramaId());
						puestoRepository.save(puesto);
						return ParametrosUtil.setearResponse(response, Boolean.TRUE, Constantes.MENSAJE_EXITOSO_EDITAR_PUESTO);
					} else {
						if (valPuesto.get().getPuestoId().equals(puestoId)) {
							//validamos flag de jefe cuando es el mismo puesto
							if (!request.getPayload().getEsJefe().equals(valPuesto.get().getEsJefe())) {
								
								puesto = puestoFind.get();
								puesto.setCampoSegUpd(Constantes.ESTADO_ACTIVO, jwt.getUsuario().getUsuario(),
										Instant.now());
								
								puesto.setEntidadId(request.getPayload().getEntidadId());
								puesto.setDescripcion(request.getPayload().getDescripcion().toUpperCase());
								puesto.setEsJefe(request.getPayload().getEsJefe());
								puesto.setOrganigramaId(request.getPayload().getOrganigramaId());
								puestoRepository.save(puesto);
								return ParametrosUtil.setearResponse(response, Boolean.TRUE, Constantes.MENSAJE_EXITOSO_EDITAR_PUESTO);
							} else {
								return ParametrosUtil.setearResponse(response, Boolean.FALSE,
										Constantes.MSG_YA_EXISTE_PUESTO_REGISTRADO_MISMA_DESC);
							}
						} else {
							return ParametrosUtil.setearResponse(response, Boolean.FALSE,
									"Ya existe un puesto registrado en la UO con la misma descripción");
						}
					}
					
				} else {
					return ParametrosUtil.setearResponse(response, Boolean.FALSE,
							Constantes.MSG_NO_EXISTE_EL_PUESTO_INGRESADO);
				}
			} else {
				
				return ParametrosUtil.setearResponse(response, Boolean.FALSE,
						Constantes.MSG_NO_EXISTE_EL_PUESTO_INGRESADO);

			}


		} catch (Exception e) {
			response = ParametrosUtil.setearResponse(response, Boolean.FALSE,
					Constantes.MENSAJE_ERROR_REGISTRO_PUESTO + " " + e.getMessage());
		}
		return response;
	}

	@Override
	public RespBase<Object> obtenerListaPuestosfromExcel(InputStream uploadedInputStream, Long entidadId, MyJsonWebToken jwt) {
		RespBase<Object> response = new RespBase<>();
		RespBase<RespPuesto> responsePuesto = new RespBase<>();
		boolean avisarObservacion = false;
		boolean avisarObservacionNegocio= false;
		ReqBase<ReqRegistrarPuesto> puestoReq = new ReqBase<>();
		try {
			XSSFWorkbook workbook = new XSSFWorkbook(uploadedInputStream);
			XSSFSheet hojaDatos = workbook.getSheet("DATOS");
			XSSFSheet hojaCodigoCombos = workbook.getSheet("CODIGO_COMBO");

			List<PuestoDatosDTO> listaDatos = new ArrayList<>();

			int ultimaFila = hojaDatos.getLastRowNum();
			int columnas = 3;
			for (int f = 1; f <=ultimaFila; f++) {
				XSSFRow filaInicio = hojaCodigoCombos.getRow(f);
				PuestoDatosDTO data = new PuestoDatosDTO();
				for (int c = 0; c < columnas; c++) {
					XSSFCell celda = filaInicio.getCell(c);
					if(celda == null) {
						celda = filaInicio.createCell(c);
						adapterPuesto.obtenerDatos(celda, data);
					}else {
						adapterPuesto.obtenerDatos(celda, data);
					}
				}
				listaDatos.add(data);
			}
			avisarObservacion = adapterPuesto.validarCombosVacios(avisarObservacion, listaDatos);
			byte[] archivoObservado = adapterPuesto.excelObservado(listaDatos, workbook, avisarObservacion);

			if (archivoObservado.length == 0) {
				List<Organigrama> lstOrganigrama = organigramaRepository.findByEntidadId(entidadId);
//				List<Puesto> listaPuesto = puestoRepository.findByEntidadId(entidadId);
				if (!CollectionUtils.isEmpty(lstOrganigrama)) {
					for (PuestoDatosDTO datos : listaDatos) {
						StringBuilder resultadosObservados = new StringBuilder();
						boolean existeOrgano = lstOrganigrama.stream().anyMatch(o -> o.getOrganigramaId().equals(datos.getUnidadOrganicaId()));
						if (existeOrgano) {
							Optional<Puesto> valPuesto = puestoRepository.findByEntidadIdAndDescripcionAndOrganigramaIdAndEstadoRegistro(
									entidadId, StringUtils.trimToEmpty(datos.getPuesto()).toUpperCase(),
									datos.getUnidadOrganicaId(), Constantes.ESTADO_ACTIVO);
							if (!valPuesto.isPresent()) {
//							boolean existePuestoBd = listaPuesto.stream().anyMatch(p -> StringUtils.trimToEmpty(p.getDescripcion()).equalsIgnoreCase(StringUtils.trimToEmpty(datos.getPuesto())));
//							if (!existePuestoBd) {
								long nroRepetidasExcel = listaDatos.stream().filter(s -> s.getPuesto().equalsIgnoreCase(datos.getPuesto()))
										.filter(o -> o.getUnidadOrganicaId().equals(datos.getUnidadOrganicaId())).count();
								if (nroRepetidasExcel == 1) {
									ReqBase<ReqRegistrarPuesto> puestoRequest =adapterPuesto.adapToBeanRegistrarPuesto(entidadId, puestoReq, datos);
									responsePuesto  = registrarPuesto(puestoRequest, jwt);
									if (Boolean.FALSE.equals(responsePuesto.getStatus().getSuccess())) {
										String mensajeResultado = "";
										for (String mensaje : response.getStatus().getError().getMessages()) {
											mensajeResultado = mensaje + ",";
										}
										mensajeResultado = mensajeResultado.trim().substring(0,mensajeResultado.length() - 1);
										resultadosObservados.append(mensajeResultado);
										avisarObservacionNegocio = true;
										response = ParametrosUtil.setearResponse(response, Boolean.FALSE,datos.getObservacionResultado());
									} 
								} else {
									resultadosObservados.append("El puesto " + datos.getPuesto() + " se repite más de una vez, ");
									avisarObservacionNegocio= true;
								}
							}else {
								resultadosObservados.append("El puesto " + datos.getPuesto() + " ya existe en esta unidad organica");
								avisarObservacionNegocio= true;
							}
							
						}else {
							resultadosObservados.append("El código del Organo / UO no esta registrado previamente.");
							avisarObservacionNegocio=true;
						}
						datos.setObservacionResultado( StringUtils.trimToEmpty(resultadosObservados.toString()));	
					}
					if (avisarObservacionNegocio) {
						byte[] archivoObservadoNegocio= adapterPuesto.excelObservado(listaDatos, workbook, avisarObservacionNegocio);
						return responseObservacion(response, listaDatos, archivoObservadoNegocio);
					}
				}else {
					return ParametrosUtil.setearResponse(response, Boolean.FALSE,"No existe Organos / Unidades Organicas registrados para esta entidad. ");
				}	
				Map<String, Object> devol = new HashMap<>();
				devol.put("archivo", Constantes.VACIO);
				devol.put("Segmento", listaDatos);
				response.setPayload(devol);
				response.getStatus().setSuccess(true);
			} else {
				return responseObservacion(response, listaDatos, archivoObservado);
			}
			workbook.close();
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return response;
	}

	private RespBase<Object> responseObservacion(RespBase<Object> response, List<PuestoDatosDTO> listaDatos,
			byte[] archivoObservadoNegocio) {
		Map<String, Object> devol = new HashMap<>();
		devol.put("archivo", Base64.getEncoder().encodeToString(archivoObservadoNegocio));
		devol.put("Segmento", listaDatos);
		response.setPayload(devol);
		response.getStatus().setSuccess(true);
		return response;
	}

	@Override
	public RespBase<RespListarPuesto> listarPuesto(Map<String, Object> parametroMap) throws ValidationException {
		try {
			List<ListaPuestoDTO> resultado = puestoRepository2.listaPuesto(parametroMap);
			RespListarPuesto respPayload = new RespListarPuesto();
			respPayload.setListaPuesto(resultado);
			return new RespBase<RespListarPuesto>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespListarDetalleUO> listarPuestosUOxEvaluador(Map<String, Object> parametroMap)
			throws ValidationException {
		try {
			List<PuestoUOxEvaluador> listaPuesto = new ArrayList<>();

			List<DetUnidadOrganica> detUO = puestoDetalleUORepository
					.listaPuestoUOEvaluador((Long) parametroMap.get("personaId"), (Long) parametroMap.get("entidadId"));
			for (DetUnidadOrganica det : detUO) {
				PuestoUOxEvaluador puestoUO = new PuestoUOxEvaluador();

				
				
				Organigrama organigrama = organigramaRepository.findByOrganigramaId(det.getOrganigramaId());

				puestoUO.setDescripcion(organigrama.getDescripcion());
				puestoUO.setSigla(organigrama.getSigla());			
				puestoUO.setDetalleUoId(det.getDetUnidadOrganicaId());
				puestoUO.setOrganigramaId(det.getOrganigramaId());

				puestoUO.setEntidadId(det.getEntidadId());
				puestoUO.setPersonaId(det.getPersonaId());
				puestoUO.setPuestoId(det.getPuestoId());
				puestoUO.setResponsable(det.getResponsable());
				puestoUO.setRolId(det.getRolId());
				listaPuesto.add(puestoUO);

			}

			RespListarDetalleUO respPayload = new RespListarDetalleUO();
			respPayload.setListaPuesto(listaPuesto);
			return new RespBase<RespListarDetalleUO>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<RespListarDetalleUO> listarPuestosUOxEvaluadorGDR(Map<String, Object> parametroMap)
			throws ValidationException {
		try {
			List<PuestoUOxEvaluador> listaPuesto = new ArrayList<>();
			List<DetUnidadOrganica> detUOGDR = puestoDetalleUORepository
					.listaPuestoUOEvaluadoGdr((Long) parametroMap.get("personaId"), (Long) parametroMap.get("entidadId"));
			
			if (detUOGDR.size()>0) {
				
				for (DetUnidadOrganica detGDR : detUOGDR) {
					
					Long detUnidadOrganicaId = detGDR.getDetUnidadOrganicaId();
				
					for (DetUnidadOrganica det : detUOGDR) {
						
						PuestoUOxEvaluador puestoUO = new PuestoUOxEvaluador();

						
						
						Organigrama organigrama = organigramaRepository.findByOrganigramaId(det.getOrganigramaId());

						puestoUO.setDescripcion(organigrama.getDescripcion());
						puestoUO.setSigla(organigrama.getSigla());						
						puestoUO.setDetalleUoId(det.getDetUnidadOrganicaId());
						puestoUO.setOrganigramaId(det.getOrganigramaId());
						puestoUO.setEntidadId(det.getEntidadId());
						puestoUO.setPersonaId(det.getPersonaId());
						puestoUO.setPuestoId(det.getPuestoId());
						puestoUO.setResponsable(det.getResponsable());
						puestoUO.setRolId(det.getRolId());
						puestoUO.setEvaluadoDetalleUoId(detUnidadOrganicaId);
						listaPuesto.add(puestoUO);

					}

				}
				
			}
			
			
			RespListarDetalleUO respPayload = new RespListarDetalleUO();
			respPayload.setListaPuesto(listaPuesto);
			return new RespBase<RespListarDetalleUO>().ok(respPayload);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw e;
		}
	}

	@Override
	public RespBase<Object> descargarExcelPuesto(Long idEntidad) {
		int indice = 1;
		int indiceFilaParametro = 1;

		//String pathFileOrg = System.getProperty("user.home") + variablesSistema.rutaExcelWindowPuesto;
		String pathFileOrg = variablesSistema.rutaExcelLinuxPuesto;

		ByteArrayOutputStream salida = new ByteArrayOutputStream();
		RespBase<Object> response = new RespBase<>();
		try (InputStream inputStream = new FileInputStream((new File(pathFileOrg)))) {

			Map<String, Object> parametroMap = new HashMap<>();
			parametroMap.put(Constantes.ENTIDADID, idEntidad);

			RespBase<RespComboUnidadOrganica> listaUnidOrganicas = organigramaService
					.buscarUnidadesOrganicasPorEntidad(parametroMap);
			RespBase<RespObtieneLista> respuestParametro = maestraApiClient.obtieneParametros(Constantes.ES_JEFE_UO);

			XSSFWorkbook workbook = new XSSFWorkbook(inputStream);
			XSSFSheet puestoData = workbook.getSheet("BD");
			createDatosExcel(indice, indiceFilaParametro, listaUnidOrganicas.getPayload().getListaComboUnidadOrganica(),
					respuestParametro.getPayload().getItems(), puestoData);
			workbook.write(salida);
			workbook.close();

			response.getStatus().setSuccess(Boolean.TRUE);
			response.setPayload(Base64.getEncoder().encodeToString(salida.toByteArray()));

		} catch (FileNotFoundException e) {
			LOGGER.error(e.getMessage(), e.getCause());
			return ParametrosUtil.setearResponse(response, Boolean.FALSE,
					"planitlla excel no encontrado " + pathFileOrg);
		} catch (IOException e) {
			LOGGER.error(e.getMessage(), e.getCause());
			return ParametrosUtil.setearResponse(response, Boolean.FALSE,
					"Ha ocurrido un error al procesar el formato.");
		} catch (FeignException e) {
			LOGGER.info("Ha ocurrido un error al invocar el Api Entidad " + e.getMessage());
			return ParametrosUtil.setearResponse(response, Boolean.FALSE,
					"Ha ocurrido un error al invocar el Api Entidad " + e.getMessage());
		}

		return response;
	}

	private void createDatosExcel(int indice, int indiceFilaParametro,
			List<ComboUnidadOrganica> listaComboUnidadOrganica,
			List<pe.gob.servir.entidad.response.RespObtieneLista.Parametro> items, XSSFSheet puestoData) {

		for (ComboUnidadOrganica unidadOr : listaComboUnidadOrganica) {
			Row fila = puestoData.createRow(indice);
			if (fila == null)
				fila = puestoData.createRow(indice);

			Cell celdaUnidadOragnicaId = fila.createCell(0);
			celdaUnidadOragnicaId.setCellValue(unidadOr.getId());

			Cell celdaSigla = fila.createCell(1);
			celdaSigla.setCellValue(unidadOr.getSigla());
			indice++;
		}

		for (pe.gob.servir.entidad.response.RespObtieneLista.Parametro datosParametro : items) {
			Row fila = puestoData.getRow(indiceFilaParametro);
			if (fila == null)
				fila = puestoData.createRow(indiceFilaParametro);

			Cell celdaJefeId = fila.createCell(2);
			celdaJefeId.setCellValue(datosParametro.getCodigoNumero());

			Cell celdaJefeDescripcion = fila.createCell(3);
			celdaJefeDescripcion.setCellValue(datosParametro.getValorTexto());
			indiceFilaParametro++;

		}

	}

}
