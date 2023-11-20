package pe.gob.servir.entidad.controller;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import javax.xml.bind.DatatypeConverter;

import org.apache.commons.lang3.StringUtils;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.model.Generico;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.UnidadOrganicaExcelDTO;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespObtenerOrganigrama;
import pe.gob.servir.entidad.response.RespObtenerUnidadOrganica;
import pe.gob.servir.entidad.response.RespOrganigrama;
import pe.gob.servir.entidad.response.RespPaises;
import pe.gob.servir.entidad.response.RespParametro;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.service.GeneralService;
import pe.gob.servir.entidad.service.OrganigramaService;
import pe.gob.servir.entidad.service.UnidadOrganicaService;
import pe.gob.servir.entidad.util.ExcelUtil;
import pe.gob.servir.entidad.util.JsonUtil;
import pe.gob.servir.entidad.util.ParametrosUtil;

@RestController
@Tag(name = "UnidadOrganica", description = "")
public class UnidadOrganicaController {
	
	private static final Logger LOGGER = Logger.getLogger(UnidadOrganicaController.class);
	@Autowired
	private UnidadOrganicaService unidadOrganicaService;

	@Autowired
	private HttpServletRequest httpServletRequest;
	
	@Autowired
	private GeneralService generalService;
	
	@Autowired
	private VariablesSistema variablesSistema;
	
	@Autowired
	private OrganigramaService organigramaService;
	
	@Operation(summary = "Crea una Unidad Organica", description = "Crea una Unidad Organica", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/unidadorganica" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespOrganigrama>> registrarUnidadOrganica(@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqOrganigrama> request) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");
		RespBase<RespOrganigrama> response = unidadOrganicaService.guardarUnidadOrganica(request, jwt, null);
		return ResponseEntity.ok(response);

	}

	@Operation(summary = Constantes.SUM_OBT_LIST + "Unidada Organica By entidadId", description = Constantes.SUM_OBT_LIST
			+ "organigrama by filtros", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/unidadorganica/{entidadId}" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<RespObtenerUnidadOrganica>> buscarUnidadOrganica(@PathVariable String access,			
			@PathVariable Long entidadId
			) {
		RespBase<RespObtenerUnidadOrganica> response;		
		response = unidadOrganicaService.buscarUnidadOrganica(entidadId);
		return ResponseEntity.ok(response);
	}
	
	@Operation(summary = "Actualizar Unidad Organica", description = "Actualizar Unidad Organica", tags = { "" },
			security = { @SecurityRequirement(name = Constantes.BEARER_JWT)})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PutMapping(path = { Constantes.BASE_ENDPOINT+"/unidadorganica/{idUnidadOrganica}" }, 
				 consumes = {MediaType.APPLICATION_JSON_VALUE },
				 produces = { MediaType.APPLICATION_JSON_VALUE })	
		public ResponseEntity<RespBase<RespOrganigrama>> actualizarUnidadOrganica(
			@PathVariable String access,
			@Valid @RequestBody ReqBase<ReqOrganigrama> request,
			@PathVariable Long idUnidadOrganica) {
		MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");	
		RespBase<RespOrganigrama> response = unidadOrganicaService.guardarUnidadOrganica(request, jwt, idUnidadOrganica);
		return ResponseEntity.ok(response);	
	}
	
	@Operation(summary = Constantes.SUM_OBT_LIST + "descargar Excel", description = Constantes.SUM_OBT_LIST
			+ "descargar Excel", tags = {
					"" }, security = { @SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@GetMapping(path = { Constantes.BASE_ENDPOINT + "/unidadorganica/downloadFormat" }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> downloadFormat(@PathVariable String access,@RequestParam(value="idEntidad", required = false) Long idEntidad
			) {
		try {	
	
			String rutaA = variablesSistema.rutaExcelLinuxUnidOrganica;

			try(InputStream uploadedInputStream = new FileInputStream((new File(rutaA)))){
				Map<String,Object> parametroMap  = new  HashMap<>();
				parametroMap.put("entidadId", idEntidad);
				RespBase<RespParametro> listaEstado = generalService.comboEstado();
				RespBase<RespParametro> listaNivel = generalService.comboNivel(String.valueOf(variablesSistema.tipoiUnidadOrganica));				
				RespBase<RespParametro> listaTipoDoc = generalService.comboTipoDocumento();
				RespBase<RespObtenerOrganigrama> listaOrgano = organigramaService.buscarOrganigramaByFilter(parametroMap);
				RespBase<RespPaises> listaPaises = generalService.comboPaises();
				listaTipoDoc = ParametrosUtil.listaTipoDNI_CE(listaTipoDoc.getPayload().getListaParametros());				
				
				List<Generico> listaEstadoGen= ParametrosUtil.convertirEstadoAGenerico(listaEstado.getPayload().getListaParametros());
				List<Generico> listaNivelGen= ParametrosUtil.convertirNivelAGenerico(listaNivel.getPayload().getListaParametros());				
				List<Generico> listaTipoDocGen= ParametrosUtil.convertirTipoDocumentoAGenerico(listaTipoDoc.getPayload().getListaParametros());
				List<Generico> listaOrganoGen= ParametrosUtil.convertirOrganoAGenerico(listaOrgano.getPayload().getListaOrganigrama());
				List<Generico> listaPaisesGen= ParametrosUtil.convertirPaisesAGenerico(listaPaises.getPayload().getListaPaises());
				
				Map<String,Object> mapaLista = new LinkedHashMap<>();			
				mapaLista.put("ESTADO", listaEstadoGen);						
				mapaLista.put("NIVEL", listaNivelGen);				
				mapaLista.put("ORGANO", listaOrganoGen);
				mapaLista.put("TIPOS DOCUMENTOS",listaTipoDocGen);
				mapaLista.put("PAISES",listaPaisesGen);
				
				byte[] arrayExcel =  ExcelUtil.updateDropDownXLSX(uploadedInputStream,mapaLista);
				RespBase<Object> response = new RespBase<>();
				response.getStatus().setSuccess(Boolean.TRUE);
				response.setPayload(Base64.getEncoder().encodeToString(arrayExcel));
			
				
				return ResponseEntity.ok(response);				
				
			}	        
		} catch (Exception e) {
			
			RespBase<Object> response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status =  HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Operation(summary = "Crea unidad organica masivo", description = "Crea unidad organica masivo", tags = { "" }, security = {
			@SecurityRequirement(name = Constantes.BEARER_JWT) })
	@ApiResponses(value = {
			@ApiResponse(responseCode = Constantes.SERVER_200, description = Constantes.OPERACION_EXITOSA),
			@ApiResponse(responseCode = Constantes.SERVER_400, description = Constantes.ERROR_VALIDACION, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }),
			@ApiResponse(responseCode = Constantes.SERVER_500, description = Constantes.ERROR_INTERNO, content = {
					@Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RespBase.class)) }) })
	@PostMapping(path = { Constantes.BASE_ENDPOINT + "/unidadorganica/masivo" }, consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<RespBase<Object>> registrarOrganigramaMasivo(@PathVariable String access,
			@RequestBody String tramaJSON) {
		RespBase<Object> response = new RespBase<>();
		ExcelUtil<UnidadOrganicaExcelDTO> validacion = new ExcelUtil(UnidadOrganicaExcelDTO::new);
		try {			
			MyJsonWebToken jwt = (MyJsonWebToken) httpServletRequest.getAttribute("jwt");				

			Map<String, Object> parametros = JsonUtil.convertirCadenaJsonPostAObjeto(tramaJSON, Map.class);			
			String base64 = parametros.get("value").toString();	
			Long entidadId = Long.parseLong(parametros.get("entidadId").toString());
			byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
			InputStream uploadedInputStream = new ByteArrayInputStream(bytes);
			InputStream uploadedInputStreamObser = new ByteArrayInputStream(bytes);
			
			List<UnidadOrganicaExcelDTO> lista = unidadOrganicaService.validarCargaMasivaUnidadOrganica(uploadedInputStream);
			response = unidadOrganicaService.cargaMasivaUnidadOrganica(jwt,  lista,entidadId);
			String fileBase64 = validacion.addObservacionesXLSXBase64(uploadedInputStreamObser, lista, true);
			
			for (int i = lista.size()-1; i >= 0; i--) {
				if(StringUtils.isEmpty(lista.get(i).getObservacionResultado())){
					lista.remove(i);
				}else{
					String observacion = "FILA " + (i+2) + " : " + lista.get(i).getObservacionResultado();
					lista.get(i).setFilaObservacion(observacion);
				}
			}
			
			response.getStatus().setSuccess(Boolean.TRUE);
			Map<String, Object> devol = new HashMap<>();
			devol.put("archivo", fileBase64);
			devol.put("unidadOrganica", lista);
			response.setPayload(devol);
			
			return ResponseEntity.ok(response);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			response = new RespBase<>();
			response.getStatus().setSuccess(Boolean.FALSE);
			response.getStatus().getError().getMessages().add(e.getMessage());
			HttpStatus status =  HttpStatus.INTERNAL_SERVER_ERROR;
			return ResponseEntity.status(status).body(response);
		}
	}

}
