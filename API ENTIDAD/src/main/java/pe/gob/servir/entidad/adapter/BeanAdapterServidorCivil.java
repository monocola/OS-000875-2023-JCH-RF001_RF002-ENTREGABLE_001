package pe.gob.servir.entidad.adapter;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.validation.Valid;

import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jboss.logging.Logger;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.api.dto.ApiBuscarCorreo;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO.Correo;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO.Documentos;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO.PersonaNatural;
import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO.Telefono;
import pe.gob.servir.entidad.api.dto.ApiSeguridadRequestDTO;
import pe.gob.servir.entidad.api.dto.AsignaRolRequestDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.CorreoApiDTO;
import pe.gob.servir.entidad.model.DatosPersonalesServidorCivilDTO;
import pe.gob.servir.entidad.model.DetUnidadOrganica;
import pe.gob.servir.entidad.model.DetalleUoDTO;
import pe.gob.servir.entidad.model.EmpleadoDTO;
import pe.gob.servir.entidad.model.PuestoDTO;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.EditParticipanteGDRDTO;
import pe.gob.servir.entidad.request.dto.PuestoActualizarDTO;
import pe.gob.servir.entidad.request.dto.PuestoAgregarDTO;
import pe.gob.servir.entidad.request.dto.RolesDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilExcelDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilGDRDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespPersonaServidorCivil;
import pe.gob.servir.entidad.security.MyJsonWebToken;
import pe.gob.servir.entidad.util.FilesUtil;

@Component
public class BeanAdapterServidorCivil {
	
	private static final Logger LOGGER = Logger.getLogger(BeanAdapterServidorCivil.class);

	public RespBase<ApiSeguridadRequestDTO> adapToRespBaseUsuario(ServidorCivilGDRDTO bean,
			RespBase<RespApiPersona> responsePersona) {
		RespBase<ApiSeguridadRequestDTO> requestUsuario = new RespBase<>();
		String password = null;
		ApiSeguridadRequestDTO usuario = new ApiSeguridadRequestDTO(StringUtils.trimToEmpty(bean.getNumeroDocumento()),
				StringUtils.trimToEmpty(bean.getCorreoElectronico()),
				responsePersona.getPayload().getPersona().getPersonaId(), null, password);
		requestUsuario.setPayload(usuario);
		return requestUsuario;
	}

	public RespBase<AsignaRolRequestDTO> adapToRespBaseRol(Long usuarioId, RolesDTO rolesId) {

		RespBase<AsignaRolRequestDTO> requestRol = new RespBase<>();
		AsignaRolRequestDTO requestRolIn = new AsignaRolRequestDTO();
		requestRolIn.setUsuarioId(usuarioId);
		requestRolIn.setRolId(rolesId.getRolesId());
		requestRol.setPayload(requestRolIn);
		return requestRol;
	}

	public PuestoDTO adapToBeanPuesto(Long entidadId, ServidorCivilGDRDTO bean, MyJsonWebToken jwt) {

		PuestoDTO requestPuesto = new PuestoDTO();
		requestPuesto.setPuestoId(bean.getPuestoId());
		requestPuesto.setEntidadId(entidadId);
		requestPuesto.setDecripcion(StringUtils.trimToEmpty(bean.getPuestoDescripcion().toUpperCase()));
		requestPuesto.setEstadoRegistro(Constantes.ESTADO_ACTIVO);
		requestPuesto.setUsuarioCreacion(StringUtils.trimToEmpty(jwt.getUsuario().getUsuario()));
		requestPuesto.setFechaCreacion(LocalDateTime.now());
		return requestPuesto;
	}

	public EmpleadoDTO adapToBeanEmpelado(ServidorCivilGDRDTO bean, MyJsonWebToken jwt,
			@Valid ReqBase<BeanServidorCivilDTO> request, Long puestoId, RespBase<RespApiPersona> responsePersona) {

		EmpleadoDTO requestEmpleado = new EmpleadoDTO();
		requestEmpleado.setEntidadId(request.getPayload().getEntidadId());
		requestEmpleado.setPersonaId(responsePersona.getPayload().getPersonaNatural().getPersonaId());
		requestEmpleado.setRegimenLaboral(bean.getRegimenLaboralId());
		requestEmpleado.setPuestoId(puestoId);
		requestEmpleado.setSindicatoFlag(StringUtils.trimToEmpty(bean.getSindicatoId()));
		requestEmpleado.setEstadoRegistro(Constantes.ESTADO_ACTIVO);
		requestEmpleado.setUsuarioCreacion(StringUtils.trimToEmpty(jwt.getUsuario().getUsuario()));
		requestEmpleado.setFechaCreacion(Instant.now());
		return requestEmpleado;
	}

	public DetalleUoDTO adapToBeanDetalleUo(ServidorCivilGDRDTO bean, MyJsonWebToken jwt, PuestoDTO requestPuesto,
			@Valid ReqBase<BeanServidorCivilDTO> request, RespBase<RespApiPersona> responsePersona) {

		DetalleUoDTO requestDetalleUo = new DetalleUoDTO();
		requestDetalleUo.setOrganigramaId(bean.getOrganoId());
		requestDetalleUo.setEntidadId(request.getPayload().getEntidadId());
		requestDetalleUo.setPersonaId(responsePersona.getPayload().getPersonaNatural().getPersonaId());
		requestDetalleUo.setPuestoId(requestPuesto.getPuestoId());
		requestDetalleUo.setEstadoRegistro(Constantes.ESTADO_ACTIVO);
		requestDetalleUo.setUsuarioCreacion(jwt.getUsuario().getUsuario());
		requestDetalleUo.setFechaCreacion(LocalDate.now());
		requestDetalleUo.setPuestoFechaInicio(bean.getFechaInicio());
		requestDetalleUo.setExcluye(StringUtils.trimToEmpty(request.getPayload().getExcluye()));
		requestDetalleUo.setResponsable(StringUtils.trimToEmpty(bean.getResponsable()));
		requestDetalleUo.setTipoAsignacion(bean.getTipoAsignacion());
		return requestDetalleUo;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public RespBase<ApiPersonaRequestDTO> adapToBeanPersona(ServidorCivilGDRDTO bean) {

		RespBase<ApiPersonaRequestDTO> requestApiPersona = new RespBase<>();

		ApiPersonaRequestDTO requestPersonaDTO = new ApiPersonaRequestDTO<>();

		PersonaNatural requestPersona = new PersonaNatural();
		requestPersona.setNombres(StringUtils.trimToEmpty(bean.getNombres()));
		requestPersona.setApellidoPaterno(StringUtils.trimToEmpty(bean.getApellidoPaterno()));
		requestPersona.setApellidoMaterno(StringUtils.trimToEmpty(bean.getApellidoMaterno()));
		requestPersona.setSexo(StringUtils.trimToEmpty(bean.getSexo()));
		requestPersona.setFechaNacimiento(bean.getFechaNacimiento());
		if(Constantes.TIPO_DOCUMENTO_CE.equals(bean.getTipoDocumento().longValue())) {
			requestPersona.setPaisId(Constantes.PAIS_DEFAULT);
		}

		List<Documentos> listaRequestDocumento = new ArrayList<>();
		Documentos documentos = new Documentos();
		documentos.setTipoDocumento(bean.getTipoDocumento());
		documentos.setNumeroDocumento(StringUtils.trimToEmpty(bean.getNumeroDocumento()));
		listaRequestDocumento.add(documentos);

		List<Correo> listaCorreo = new ArrayList<>();
		Correo requestCorreo = new Correo();
		requestCorreo.setCorreo(StringUtils.trimToEmpty(bean.getCorreoElectronico()));
		listaCorreo.add(requestCorreo);

		requestPersonaDTO.setPersona(requestPersona);
		requestPersonaDTO.setDocumentos(listaRequestDocumento);
		requestPersonaDTO.setCorreos(listaCorreo);

		requestApiPersona.setPayload(requestPersonaDTO);

		return requestApiPersona;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public RespBase<ApiPersonaRequestDTO> adapToBeanPersona(ServidorCivilExcelDTO bean) {

		RespBase<ApiPersonaRequestDTO> requestApiPersona = new RespBase<>();

		ApiPersonaRequestDTO requestPersonaDTO = new ApiPersonaRequestDTO<>();

		PersonaNatural requestPersona = new PersonaNatural();
		requestPersona.setNombres(StringUtils.trimToEmpty(bean.getNombres()));
		requestPersona.setApellidoPaterno(StringUtils.trimToEmpty(bean.getApellidoPaterno()));
		requestPersona.setApellidoMaterno(StringUtils.trimToEmpty(bean.getApellidoMaterno()));
		requestPersona.setSexo(StringUtils.trimToEmpty(bean.getSexoId()));
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constantes.FORMATO_FECHA_DD_MM_YYYY);
		requestPersona.setFechaNacimiento(LocalDate.parse(bean.getFechaNacimiento(), formatter));
		if (Constantes.TIPO_DOCUMENTO_CE.equals(Long.valueOf(bean.getDocumentoId()))) {
			requestPersona.setPaisId(Constantes.PAIS_DEFAULT);
		}

		List<Documentos> listaRequestDocumento = new ArrayList<>();
		Documentos documentos = new Documentos();
		documentos.setTipoDocumento(Integer.valueOf(bean.getDocumentoId()));
		documentos.setNumeroDocumento(StringUtils.trimToEmpty(bean.getNumeroDocumento()));
		listaRequestDocumento.add(documentos);

		List<Correo> listaCorreo = new ArrayList<>();
		Correo requestCorreo = new Correo();
		requestCorreo.setCorreo(StringUtils.trimToEmpty(bean.getCorreoLaboral()));
		listaCorreo.add(requestCorreo);

		requestPersonaDTO.setPersona(requestPersona);
		requestPersonaDTO.setDocumentos(listaRequestDocumento);
		requestPersonaDTO.setCorreos(listaCorreo);

		requestApiPersona.setPayload(requestPersonaDTO);

		return requestApiPersona;
	}

	@SuppressWarnings({ "rawtypes" })
	public RespBase<ApiBuscarCorreo> adapToBeanCorreo(ServidorCivilGDRDTO bean) {

		RespBase<ApiBuscarCorreo> requestApiPersona = new RespBase<>();

		ApiBuscarCorreo requestPersonaDTO = new ApiBuscarCorreo<>();

		requestPersonaDTO.setTipoCorreo("PRINC");
		requestPersonaDTO.setCorreoId(null);
		requestPersonaDTO.setCorreo(bean.getCorreoElectronico());
		requestApiPersona.setPayload(requestPersonaDTO);

		return requestApiPersona;
	}

	
	public RespBase<ApiPersonaRequestDTO.Telefono> adapToBeanTelefono(RespBase<RespApiPersona.Telefono> bean) {

		RespBase<ApiPersonaRequestDTO.Telefono> requestApiTelefono = new RespBase<>();

		Telefono requestTelefono = new Telefono();
		requestTelefono.setTelefonoId(bean.getPayload().getTelefonoId());
		requestTelefono.setTipoTelefono(bean.getPayload().getTipoTelefono());
		requestTelefono.setCodigoArea(bean.getPayload().getCodigoArea());
		requestTelefono.setNumeroTelefono(bean.getPayload().getNumeroTelefono());
		requestTelefono.setNumeroAnexo(bean.getPayload().getNumeroAnexo());

		requestApiTelefono.setPayload(requestTelefono);

		return requestApiTelefono;
	}

	public Map<String, Object> adapToParamterValidacionAgregarPuesto(PuestoAgregarDTO bean) {

		Map<String, Object> parametro = new HashedMap<>();
		PuestoActualizarDTO beanUpdate = new PuestoActualizarDTO();
		
		beanUpdate.setUoId(bean.getUoId());
		beanUpdate.setFechaInicio(bean.getFechaInicio());
		beanUpdate.setTipoAsignacion(bean.getTipoAsignacion());
		beanUpdate.setPersonaIdAsignada(bean.getPersonaIdAsignada());
		
		parametro = validacionDetallePuesto(beanUpdate);

		return parametro;
	}

	public Map<String, Object> adapToParamterValidacionActualizarDetllePuesto(PuestoActualizarDTO bean) {

		Map<String, Object> parametro = new HashedMap<>();
		parametro = validacionDetallePuesto(bean);
		
		return parametro;
	}
	
	private Map<String, Object> validacionDetallePuesto(PuestoActualizarDTO bean) {

		Map<String, Object> parametro = new HashedMap<>();
		String estado = "";
		String mensaje = "";

		if (bean.getUoId() == null)
			mensaje = " NO SE ENCONTRO LA UNIDAD ORGANICA EN EL SERVIDOR CIVIL";
		if (bean.getFechaInicio() == null)
			mensaje = " NO SE ENCONTRO LA FECHA DE INICIO EN EL SERVIDOR CIVIL";
//		if (bean.getTipoAsignacion() == null) {
//			mensaje = " NO SE ENCONTRO EL TIPO DE ASIGNACION EN EL SERVIDOR CIVIL";
//		} else {
//			if (bean.getTipoAsignacion().equals(Constantes.TIPO_ASIGNACION_ENCARGATURA)
//					&& bean.getPersonaIdAsignada() == null)
//				mensaje = " NO SE ENCONTRO LA PERSONA ASIGNADA EN EL SERVIDOR CIVIL";
//		}

		estado = mensaje.isEmpty() ? Constantes.ESTADO_ES_VALIDO : Constantes.ESTADO_NO_ES_VALIDO;
		parametro.put("estado", estado);
		parametro.put("mensaje", mensaje);

		return parametro;
	}
	
	public Date adapToDate(String strFecha) {
		Date fechaInicio = null;
		try {
			SimpleDateFormat formato = new SimpleDateFormat(Constantes.FORMATO_FECHA_AUDITORIA);
			fechaInicio = formato.parse(strFecha);
		} catch (ParseException e) {
			LOGGER.error(e.getMessage(), e);
		}
		return fechaInicio;
	}

	public String adapToString(Date fecha) throws ParseException {
		String strFecha = null;
		DateFormat formato = new SimpleDateFormat(Constantes.FORMATO_FECHA_AUDITORIA);
		strFecha = formato.format(fecha);
		return strFecha;
	}

	public ReqBase<DetUnidadOrganica> adapToRequestDetUnidadOrganica(Long idDetUOPersonaAsingada, LocalDate fechaCese) {

		ReqBase<DetUnidadOrganica> response = new ReqBase<>();
		DetUnidadOrganica unidadOrganica = new DetUnidadOrganica();
		unidadOrganica.setDetUnidadOrganicaId(idDetUOPersonaAsingada);
		Date fecha = adapToFechaDate(fechaCese);
		unidadOrganica.setFechaCesePuesto(fecha);
		response.setPayload(unidadOrganica);
		return response;
	}

	private Date adapToFechaDate(LocalDate fechaCese) {
		ZoneId systemTimeZone = ZoneId.systemDefault();
		return Date.from(fechaCese.atStartOfDay(systemTimeZone).toInstant());

	}

	public DetUnidadOrganica adapToBeanUpdateDetUnidadOrganica(@Valid EditParticipanteGDRDTO request,
			MyJsonWebToken jwt, DetUnidadOrganica detUnidadOrganica) {
		DetUnidadOrganica bean = new DetUnidadOrganica();
		bean.setDetUnidadOrganicaId(detUnidadOrganica.getDetUnidadOrganicaId());
		bean.setOrganigramaId(detUnidadOrganica.getOrganigramaId());
		bean.setEntidadId(detUnidadOrganica.getEntidadId());
		bean.setPersonaId(detUnidadOrganica.getPersonaId());
		bean.setPuestoId(detUnidadOrganica.getPuestoId());
		bean.setResponsable(detUnidadOrganica.getResponsable());
		bean.setExcluye(detUnidadOrganica.getExcluye());
		bean.setEstadoRegistro(StringUtils.trimToEmpty(detUnidadOrganica.getEstadoRegistro()));
		bean.setUsuarioCreacion(StringUtils.trimToEmpty(detUnidadOrganica.getUsuarioCreacion()));
		bean.setFechaCreacion(detUnidadOrganica.getFechaCreacion());
		bean.setUsuarioModificacion(jwt.getUsuario().getUsuario());
		bean.setFechaModificacion(Instant.now());
		bean.setFechaInicioPuesto(detUnidadOrganica.getFechaInicioPuesto());
		bean.setFechaCesePuesto(detUnidadOrganica.getFechaCesePuesto());
		bean.setTipoAsignacion(detUnidadOrganica.getTipoAsignacion());
		Long flagHabilitar = request.getFlagHabilitar() != null ? request.getFlagHabilitar()
				: detUnidadOrganica.getFlagHabilitar();
		Long estadoServidorGdrBD = detUnidadOrganica.getEstadoSrvCivGdrId() == null
				|| detUnidadOrganica.getEstadoSrvCivGdrId().equals(Constantes.ESTADO_GDR_DEFAULT)
						? Constantes.ESTADO_GDR_DEFAULT
						: detUnidadOrganica.getEstadoSrvCivGdrId();
		Long estadoSrvCivGdrId = request.getEstadoSrvCivGdrId() != null ? request.getEstadoSrvCivGdrId()
				: estadoServidorGdrBD;
		Long evaluadorId = request.getPersonaEvaluadorId() == null ? 0 : request.getPersonaEvaluadorId();
		Long personaEvaluadorId = evaluadorId != 0 ? request.getPersonaEvaluadorId()
				: detUnidadOrganica.getPersonaEvaluadorId();
		bean.setSegmentoId(
				request.getSegmentoId() != null ? request.getSegmentoId() : detUnidadOrganica.getSegmentoId());
		bean.setEstadoSrvCivGdrId(estadoSrvCivGdrId);

		if ((request.getRolId() == null || request.getRolId() == 0) || detUnidadOrganica.getRolId() == null) {
			bean.setRolId(detUnidadOrganica.getRolId());
			bean.setIndicadorMeta(request.getIndicadorMeta() != null ? request.getIndicadorMeta()
					: detUnidadOrganica.getIndicadorMeta());
		}

		if (request.getRolId() != null && request.getRolId() == 1) {
			bean.setRolId(request.getRolId());
			bean.setIndicadorMeta(0L);
		}
		if (request.getRolId() != null && request.getRolId() > 1) {
			bean.setRolId(request.getRolId());
			bean.setIndicadorMeta(request.getIndicadorMeta() != null ? request.getIndicadorMeta()
					: detUnidadOrganica.getIndicadorMeta() != null ? detUnidadOrganica.getIndicadorMeta() : 1L);
		}

		bean.setFlagHabilitar(flagHabilitar);
		bean.setPersonaEvaluadorId(personaEvaluadorId);
        if (request.getEstadoSrvCivGdrId() != null && request.getEstadoSrvCivGdrId() > 2) {
        	bean.setRolId(null);
        	bean.setSegmentoId(null);
			
		}
		return bean;
	}

	public ReqBase<DetUnidadOrganica> adapToRequestDetUnidad(Long idDetUOPersonaAsingada, Date fechaInicio) {
		ReqBase<DetUnidadOrganica> response = new ReqBase<>();
		DetUnidadOrganica unidadOrganica = new DetUnidadOrganica();
		unidadOrganica.setDetUnidadOrganicaId(idDetUOPersonaAsingada);
		unidadOrganica.setFechaCesePuesto(fechaInicio);
		response.setPayload(unidadOrganica);
		return response;
	}

	public RespApiPersona.Correo adapterCorreo(List<CorreoApiDTO> correoRpta) {
		RespApiPersona.Correo correo = new RespApiPersona.Correo();

		if (!correoRpta.isEmpty()) {
			correo.setCorreoId(correoRpta.get(0).getCorreoId());
			correo.setCorreo(correoRpta.get(0).getCorreo());
			correo.setTipoCorreo(correoRpta.get(0).getTipoCorreo());
			correo.setPersonaId(correoRpta.get(0).getPersonaId());
		}
		return correo;
	}

	public byte[] excelObservacionServidorCivil(List<ServidorCivilExcelDTO> lista,
			InputStream uploadedInputStreamObserv) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		try {
			XSSFWorkbook workbook = new XSSFWorkbook(uploadedInputStreamObserv);
			XSSFSheet hojaObservada = workbook.getSheet("DATOS");
			CellStyle color = workbook.createCellStyle();
			color.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
			color.setFillPattern(FillPatternType.DIAMONDS);
			color.setAlignment(HorizontalAlignment.CENTER);
			CellStyle estilo = workbook.createCellStyle();
			Cell cellObservacion = hojaObservada.getRow(0).createCell(14);
			cellObservacion.setCellValue("OBSERVACIÓN");
			cellObservacion.setCellStyle(estilo);
			cellObservacion.setCellStyle(color);

			for (int f = 1; f <= lista.size(); f++) {
				Cell cell = hojaObservada.createRow(1).createCell(14);
				cell.setCellValue(lista.get(0).getObservacionResultado());
				cell.setCellStyle(estilo);
			}

			workbook.write(os);
			workbook.close();
		} catch (Exception e) {
			LOGGER.info("error al escribir el excel observado" + e.getMessage());
			e.getMessage();
		}
		return os.toByteArray();
	}

	public void adapToBeanResponseServidorCivil(RespPersonaServidorCivil out,
			DatosPersonalesServidorCivilDTO inServidor) {

		out.setDetalleuoId(inServidor.getDetalleuoId());
		out.setPersonaId(inServidor.getPersonaId());
		out.setApellidoPaterno(StringUtils.trimToEmpty(inServidor.getApellidoPaterno()));
		out.setApellidoMaterno(StringUtils.trimToEmpty(inServidor.getApellidoMaterno()));
		out.setNombres(StringUtils.trimToEmpty(inServidor.getNombres()));
		out.setTipoDocumento(StringUtils.trimToEmpty(inServidor.getTipoDocumento()));
		out.setNumeroDocumento(inServidor.getNumeroDocumento());
		out.setTelefono(StringUtils.trimToEmpty(inServidor.getTelefono()));
		out.setGenero(StringUtils.trimToEmpty(inServidor.getGenero()));
		out.setFechaNacimiento(inServidor.getFechaNacimiento() == null ? null: FilesUtil.formatDateToString(inServidor.getFechaNacimiento()));
		out.setCorreoInstitucional(StringUtils.trimToEmpty(inServidor.getCorreoInstitucional()));
		out.setCorreoAlternativo(StringUtils.trimToEmpty(inServidor.getCorreoAlternativo()));
		out.setRegimenLaboral(StringUtils.trimToEmpty(inServidor.getRegimenLaboral()));
		out.setSindicato(StringUtils.trimToEmpty(inServidor.getSindicato()));
		out.setUoId(inServidor.getUoId());
		out.setRegimenId(inServidor.getRegimenId());
		
		String ruta = inServidor.getUrlFoto();
		if(Constantes.VACIO.equals(StringUtils.trimToEmpty(ruta))) {
			out.setUrlFoto(Constantes.VACIO);
		}else {
			out.setUrlFoto(inServidor.getUrlFoto());
		}
	}
}
