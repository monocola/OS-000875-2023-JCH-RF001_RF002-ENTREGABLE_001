package pe.gob.servir.entidad.adapter;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.List;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jboss.logging.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.common.VariablesSistema;
import pe.gob.servir.entidad.model.Organigrama;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqOrganigrama;
import pe.gob.servir.entidad.request.dto.BeanServidorCivilDTO;
import pe.gob.servir.entidad.request.dto.OrganigramaExcelDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilExcelDTO;
import pe.gob.servir.entidad.request.dto.ServidorCivilGDRDTO;
import pe.gob.servir.entidad.request.dto.UnidadOrganicaExcelDTO;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.util.ParametrosUtil;

@Component
public class BeanAdapterOrganigrama {

	private static final Logger LOGGER = Logger.getLogger(BeanAdapterOrganigrama.class);

	@Autowired
	private VariablesSistema variablesSistema;

	public PersonaDTO adapToPersonaDTO(ReqBase<ReqOrganigrama> request) {
		PersonaDTO personaResponsable = new PersonaDTO();

		personaResponsable.setTipoDocumento(request.getPayload().getTipoDocumento());
		personaResponsable.setNumeroDocumento(request.getPayload().getNumeroDocumento());
		personaResponsable.setNombres(request.getPayload().getNombres());
		personaResponsable.setApellidoPaterno(request.getPayload().getApellidoPaterno());
		personaResponsable.setApellidoMaterno(request.getPayload().getApellidoMaterno());
		personaResponsable.setCorreoPrincipalId(request.getPayload().getCorreoId());
		personaResponsable.setCorreoPrincipal(request.getPayload().getCorreo());
		personaResponsable.setTelefonoFijoId(request.getPayload().getTelefonoId());
		personaResponsable.setTelefonoFijo(request.getPayload().getTelefono());

		return personaResponsable;
	}

	public RespApiPersona adaptToPersonaResponse(ReqBase<ReqOrganigrama> request, Organigrama organigrama,
			RespBase<Object> responseWS) {
		RespApiPersona personaResponse = (RespApiPersona) responseWS.getPayload();
		organigrama.setPersonaResponsableId(personaResponse.getPersona().getPersonaId());
		organigrama.setTelefonoId((personaResponse.getTelefonos() != null && !personaResponse.getTelefonos().isEmpty())
				? personaResponse.getTelefonos().get(0).getTelefonoId()
				: null);
		organigrama.setCorreoId((personaResponse.getCorreos() != null && !personaResponse.getCorreos().isEmpty())
				? personaResponse.getCorreos().get(0).getCorreoId()
				: null);
		organigrama.setAreaId(request.getPayload().getAreaId());
		organigrama.setDescripcion(request.getPayload().getDescripcion());
		organigrama.setDescripcionCorta(request.getPayload().getDescripcionCorta());
		organigrama.setEntidadId(request.getPayload().getEntidadId());
		organigrama.setNaturalezaOrgano(request.getPayload().getNaturalezaOrgano());
		organigrama.setNivel(request.getPayload().getNivel());
		organigrama.setNivelGobiernoId(request.getPayload().getNivelGobiernoId());
		organigrama.setOrden(request.getPayload().getOrden());
		organigrama.setPadreOrganigramaId(request.getPayload().getPadreOrganigramaId());
		organigrama.setPuesto(request.getPayload().getPuesto());
		organigrama.setSedeId(request.getPayload().getSedeId());
		organigrama.setSigla(request.getPayload().getSigla());
		organigrama.setTipoOrganoUoId(request.getPayload().getTipoOrganoUoId());
		organigrama.setEstadoRegistro(request.getPayload().getEstadoRegistro());
		return personaResponse;
	}

	public BeanServidorCivilDTO adapToBeanOrganigramaExcel(BeanServidorCivilDTO beanServidorCivilDTO,
			ServidorCivilExcelDTO civExcelDTO, Long organoId, Long regimenLaboralId) {
		ServidorCivilGDRDTO civGDRDTO = new ServidorCivilGDRDTO();

		civGDRDTO.setTipoDocumento(Integer.parseInt(civExcelDTO.getDocumentoId().trim()));
		civGDRDTO.setNumeroDocumento(civExcelDTO.getNumeroDocumento());
		civGDRDTO.setApellidoPaterno(civExcelDTO.getApellidoPaterno().toUpperCase());
		civGDRDTO.setApellidoMaterno(civExcelDTO.getApellidoMaterno().toUpperCase());
		civGDRDTO.setNombres(civExcelDTO.getNombres().toUpperCase());
		civGDRDTO.setCorreoElectronico(civExcelDTO.getCorreoLaboral().toLowerCase().trim());
		civGDRDTO.setSexo(civExcelDTO.getSexoId().trim());
		civGDRDTO.setPuestoDescripcion(civExcelDTO.getPuesto().toUpperCase());
		civGDRDTO.setPuestoId(Long.parseLong(civExcelDTO.getPuestoId().trim()));
		civGDRDTO.setFechaNacimiento(ParametrosUtil.StringToLocalDateFechaNacimiento(civExcelDTO.getFechaNacimiento()));
		civGDRDTO.setFechaInicio(ParametrosUtil.StringToLocalDate(civExcelDTO.getFechaInicio()));
		civGDRDTO.setOrganoId(organoId);
		civGDRDTO.setRegimenLaboralId(regimenLaboralId);
		civGDRDTO.setSindicatoId(civExcelDTO.getSindicatoId());
		civGDRDTO.setResponsable(civExcelDTO.getResponsable().equalsIgnoreCase(Constantes.SI) ? "S" : "N");
		civGDRDTO.setTipoAsignacion(Constantes.TIPO_ASIGNACION_PRINCIPAL);

		beanServidorCivilDTO.setServidorCivil(civGDRDTO);
		return beanServidorCivilDTO;
	}

	public ReqBase<ReqOrganigrama> adapTobeanUnidadOrganicaPayload(Long entidadId,
			UnidadOrganicaExcelDTO unidadOrganicaExcelDto) {

		ReqBase<ReqOrganigrama> unidadOrganicaPayload = new ReqBase<>();
		ReqOrganigrama unidadOrganica = new ReqOrganigrama();
		unidadOrganica.setEntidadId(entidadId);
		unidadOrganica.setEstadoRegistro(unidadOrganicaExcelDto.getEstado().split("-")[0].trim());
		unidadOrganica.setNivel(Long.parseLong(unidadOrganicaExcelDto.getNivel().split("-")[0].trim()));
		unidadOrganica.setDescripcion(unidadOrganicaExcelDto.getNombreOrgano());
		unidadOrganica.setSigla(unidadOrganicaExcelDto.getSigla());
		unidadOrganica
				.setPadreOrganigramaId(Long.parseLong(unidadOrganicaExcelDto.getTipoOrgano().split("-")[0].trim()));
		unidadOrganica.setTipoOrganoUoId(Long.valueOf(variablesSistema.tipoiUnidadOrganica));
		unidadOrganica.setOrden(0);
		unidadOrganica
				.setTipoDocumento(Integer.parseInt(unidadOrganicaExcelDto.getTipoDocumento().split("-")[0].trim()));
		unidadOrganica.setNumeroDocumento(unidadOrganicaExcelDto.getNroDocumento());
		unidadOrganica.setNombres(unidadOrganicaExcelDto.getNombres());
		unidadOrganica.setApellidoPaterno(unidadOrganicaExcelDto.getApellidoPaterno());
		unidadOrganica.setApellidoMaterno(unidadOrganicaExcelDto.getApellidoMaterno());
		unidadOrganica.setPuesto(unidadOrganicaExcelDto.getPuesto());
		unidadOrganica.setTelefono(unidadOrganicaExcelDto.getCelular());
		unidadOrganica.setCorreo(unidadOrganicaExcelDto.getCorreoLaboral());
		unidadOrganica.setPaisId(Long.parseLong(unidadOrganicaExcelDto.getPais().split("-")[0].trim()));
		unidadOrganicaPayload.setPayload(unidadOrganica);
		return unidadOrganicaPayload;

	}

	public byte[] excelObservacionOrganigrama(List<OrganigramaExcelDTO> lista, InputStream uploadedInputStreamObserv) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		try {
			XSSFWorkbook workbook = new XSSFWorkbook(uploadedInputStreamObserv);
			XSSFSheet hojaDatos = workbook.getSheet("DATOS");
			CellStyle estilo = workbook.createCellStyle();
			for (int f = 1; f <= lista.size(); f++) {
				Cell cell = hojaDatos.createRow(1).createCell(4);
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

}
