package pe.gob.servir.entidad.adapter;

import java.io.ByteArrayOutputStream;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jboss.logging.Logger;
import org.springframework.stereotype.Component;

import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.PuestoDatosDTO;
import pe.gob.servir.entidad.request.ReqBase;
import pe.gob.servir.entidad.request.ReqRegistrarPuesto;

@Component
public class BeanAdapterPuesto {

	private static final Logger LOGGER = Logger.getLogger(BeanAdapterPuesto.class);

	public void obtenerDatos(XSSFCell celda, PuestoDatosDTO data) {

		if (celda.getColumnIndex() == 0 && celda.getCellTypeEnum() == CellType.FORMULA) {
			data.setUnidadOrganicaId((long) celda.getNumericCellValue());
		}

		if (celda.getColumnIndex() == 1 && celda.getCellTypeEnum() == CellType.FORMULA) {
			Long flagResponsable = (long) celda.getNumericCellValue();
			data.setResponsable(flagResponsable.toString());
		}
		if (celda.getColumnIndex() == 2 && celda.getCellTypeEnum() == CellType.FORMULA) {
			data.setPuesto(StringUtils.trimToEmpty(celda.getStringCellValue()));
		}

	}

	public boolean validarCombosVacios(boolean avisarObservacion, List<PuestoDatosDTO> listaDatos) {

		if (CollectionUtils.isEmpty(listaDatos)) {
			PuestoDatosDTO beanPuesto = new PuestoDatosDTO();
			beanPuesto.setObservacion(Constantes.NO_SE_INGRESO_DATOS);
			listaDatos.add(beanPuesto);
			avisarObservacion = true;
		} else {
			for (int i = 0; i < listaDatos.size(); i++) {
				StringBuilder observacion = new StringBuilder();
				if (Constantes.CERO.equals(listaDatos.get(i).getUnidadOrganicaId())) {
					observacion.append(Constantes.SELECCIONAR_UNIDAD_ORGANICA);
					avisarObservacion = true;
				}
				if (Constantes.CERO.equals(Long.parseLong(listaDatos.get(i).getResponsable()))) {
					observacion.append(Constantes.SELECCIONAR_RESPONSABLE);
					avisarObservacion = true;
				}
				if (Constantes.VACIO.equals(listaDatos.get(i).getPuesto())) {
					observacion.append(Constantes.REGISTRAR_PUESTO);
					avisarObservacion = true;
				}
				listaDatos.get(i).setObservacion(StringUtils.trimToEmpty(observacion.toString()));
			}
		}
		return avisarObservacion;
	}

	public byte[] excelObservado(List<PuestoDatosDTO> listaDatos, XSSFWorkbook workbook, boolean avisarObservacion) {

		ByteArrayOutputStream os = new ByteArrayOutputStream();
		try {
			int indiceListaDatos = 0;
			if (avisarObservacion) {
				XSSFSheet hojaObservada = workbook.getSheet("DATOS");
				CellStyle color = workbook.createCellStyle();
				color.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
				color.setFillPattern(FillPatternType.DIAMONDS);
				color.setAlignment(HorizontalAlignment.CENTER);
				CellStyle estilo = workbook.createCellStyle();
				Cell cellObservacion = hojaObservada.getRow(0).createCell(3);
				cellObservacion.setCellValue("OBSERVACIÓN");
				cellObservacion.setCellStyle(estilo);
				cellObservacion.setCellStyle(color);

				for (int f = 1; f <= listaDatos.size(); f++) {
					Row fila = hojaObservada.getRow(f);
					if(fila == null)
					{
						Cell cell = hojaObservada.createRow(f).createCell(3);
						indiceListaDatos = adapToPuestoObservacion(listaDatos, indiceListaDatos, estilo, cell);
					}else {
						Cell cell = hojaObservada.getRow(f).createCell(3);
						indiceListaDatos = adapToPuestoObservacion(listaDatos, indiceListaDatos, estilo, cell);
					}						
				}
				workbook.write(os);
				workbook.close();
			} else {
				return os.toByteArray();
			}

		} catch (Exception e) {
			LOGGER.info("error al escribir el excel observado" + e.getMessage());
		}
		return os.toByteArray();
	}

	private int adapToPuestoObservacion(List<PuestoDatosDTO> listaDatos, int indiceListaDatos, CellStyle estilo,
			Cell cell) {
		String datosFormato = StringUtils.trimToEmpty(listaDatos.get(indiceListaDatos).getObservacion());
		String datosNegocio = StringUtils.trimToEmpty(listaDatos.get(indiceListaDatos).getObservacionResultado());
		cell.setCellValue(datosFormato.equals(Constantes.VACIO) ? datosNegocio : datosFormato);
		cell.setCellStyle(estilo);
		indiceListaDatos++;
		return indiceListaDatos;
	}

	public ReqBase<ReqRegistrarPuesto> adapToBeanRegistrarPuesto(Long entidadId, ReqBase<ReqRegistrarPuesto> puestoReq,
			PuestoDatosDTO datos) {
		ReqRegistrarPuesto puesto = new ReqRegistrarPuesto();
		puesto.setEntidadId(entidadId);
		puesto.setOrganigramaId(datos.getUnidadOrganicaId());
		puesto.setDescripcion(datos.getPuesto());
		puesto.setEsJefe(datos.getResponsable().equalsIgnoreCase("1") ? "S" : "N");
		puestoReq.setPayload(puesto);
		return puestoReq;
	}

}
