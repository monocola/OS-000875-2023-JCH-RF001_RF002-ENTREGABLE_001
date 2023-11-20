package pe.gob.servir.entidad.util;

import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.DataFormatter;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jboss.logging.Logger;

import lombok.Getter;
import lombok.Setter;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.ComboPuesto;
import pe.gob.servir.entidad.model.Generico;

@Getter
@Setter
public class ExcelUtil<G> {
	private static final Logger LOGGER = Logger.getLogger(ExcelUtil.class);

	private Supplier<G> entidadGenerica;

	public ExcelUtil(Supplier<G> entidadGenerica) {
		this.entidadGenerica = entidadGenerica;
	}

	G instance() {
		return entidadGenerica.get();
	}

	public List<G> utilExcelToPojo(InputStream uploadedInputStreamExcel, int posicionHoja) {
		try {

			List<G> listaPersona = new ArrayList<>();
			List<String[]> lista = readExcelFileToArray(uploadedInputStreamExcel, posicionHoja);
			String[] cabecera = lista.get(0);
			List<String[]> listaCuerpo = lista.subList(1, lista.size());
			listaCuerpo.forEach(mc -> {
				G persona = instance();
				int i = 0;
				for (String item : cabecera) {
					item = StringUtils.defaultString(item);

					boolean resp = setFieldGeneric(persona, item, mc[i]);
					if (!resp) {
						setFieldGeneric(persona, Constantes.OBSERVACION, "error al validar");
					}
					i++;
				}
				listaPersona.add(persona);
			});
			return listaPersona;
		} catch (Exception e) {

			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}

	public List<G> utilExcelToPojo(InputStream uploadedInputStreamExcel, int[] posicionesHoja) {
		try {

			List<G> listaPersona = new ArrayList<>();

			List<String[]> lista = readExcelFileToArray(uploadedInputStreamExcel, posicionesHoja);

			String[] cabecera = lista.get(0);
			List<String[]> listaCuerpo = lista.subList(1, lista.size());
			listaCuerpo.forEach(mc -> {
				G persona = instance();
				int i = 0;
				for (String item : cabecera) {
					item = StringUtils.defaultString(item);

					boolean resp = setFieldGeneric(persona, item, mc[i]);
					if (!resp) {
						setFieldGeneric(persona, Constantes.OBSERVACION, "error al validar");
					}
					i++;
				}
				listaPersona.add(persona);
			});
			return listaPersona;
		} catch (Exception e) {

			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}

	@SuppressWarnings("resource")
	public static List<String[]> readExcelFileToArray(InputStream excelStream, int posicionHoja) {
		List<String[]> arrayDatos = new ArrayList<>();
		// Representación del más alto nivel de la hoja excel.
		try (XSSFWorkbook workbook = new XSSFWorkbook(excelStream)) {

			// Elegimos la hoja que se pasa por parámetro.
			XSSFSheet sheet = workbook.getSheetAt(posicionHoja);
			// Objeto que nos permite leer un fila de la hoja excel, y de aquí extraer el
			// contenido de las celdas.
			Row hssfRow = sheet.getRow(sheet.getTopRow());
			String[] datos = new String[hssfRow.getLastCellNum()];
			// Para este ejemplo vamos a recorrer las filas obteniendo los datos que
			// queremos
			// DataFormatter dataFormatter = new DataFormatter();
			for (Row row : sheet) {
				for (Cell cell : row) {
//					datos[cell.getColumnIndex()] = dataFormatter.formatCellValue(cell);
					datos[cell.getColumnIndex()] = obtenerCellValidada(workbook, cell);
				}
				arrayDatos.add(datos);
				datos = new String[hssfRow.getLastCellNum()];
			}

			workbook.close();
		} catch (FileNotFoundException fileNotFoundException) {
			LOGGER.error(fileNotFoundException.getMessage(), fileNotFoundException);
		} catch (IOException ex) {
			LOGGER.error(ex.getMessage(), ex);
		} finally {
			try {
				excelStream.close();
			} catch (IOException ex) {
				LOGGER.error(ex.getMessage(), ex);
			}
		}
		return arrayDatos;
	}

	@SuppressWarnings("resource")
	public static List<String[]> readExcelFileToArray(InputStream excelStream, int[] posicionHoja) {
		List<String[]> arrayDatosGeneral = new ArrayList<>();

		List<String[]> arrayDatos = new ArrayList<>();
		List<String[]> arrayDatosCodigo = new ArrayList<>();

		// Representación del más alto nivel de la hoja excel.
		try (XSSFWorkbook workbook = new XSSFWorkbook(excelStream)) {

			// Lista de Datos
			// Elegimos la hoja que se pasa por parámetro.
			XSSFSheet sheet = workbook.getSheetAt(posicionHoja[0]);
			// Objeto que nos permite leer un fila de la hoja excel, y de aquí extraer el
			// contenido de las celdas.
			Row hssfRow = sheet.getRow(sheet.getTopRow());
			String[] datos = new String[hssfRow.getLastCellNum()];
			// Para este ejemplo vamos a recorrer las filas obteniendo los datos que
			// queremos
			for (Row row : sheet) {
				for (Cell cell : row) {
					datos[cell.getColumnIndex()] = obtenerCellValidada(workbook, cell);
				}
				arrayDatos.add(datos);
				datos = new String[hssfRow.getLastCellNum()];
			}

			// Lista de Codigos
			XSSFSheet sheetCodigo = workbook.getSheetAt(posicionHoja[1]);
			int rowStart = Math.min(0, sheetCodigo.getFirstRowNum());
			int rowEnd = Math.max(0, arrayDatos.size());
			Row hssfRowCodigo = sheetCodigo.getRow(sheetCodigo.getTopRow());
			String[] datosCodigo = new String[hssfRowCodigo.getLastCellNum()];

			for (int rowNum = rowStart; rowNum < rowEnd; rowNum++) {
				Row row = sheetCodigo.getRow(rowNum);
				for (Cell cell : row) {
					datosCodigo[cell.getColumnIndex()] = obtenerCellValidada(workbook, cell);
				}
				arrayDatosCodigo.add(datosCodigo);
				datosCodigo = new String[hssfRowCodigo.getLastCellNum()];
			}

			// Unir las dos Listas en una sola
			String[] datosGeneral = new String[hssfRow.getLastCellNum() + hssfRowCodigo.getLastCellNum()];
			for (int i = 0; i < arrayDatos.size(); i++) {
				datosGeneral = ArrayUtils.addAll(arrayDatos.get(i), arrayDatosCodigo.get(i));
				arrayDatosGeneral.add(datosGeneral);
			}

		} catch (FileNotFoundException fileNotFoundException) {
			LOGGER.error(fileNotFoundException.getMessage(), fileNotFoundException);
		} catch (IOException ex) {
			LOGGER.error(ex.getMessage(), ex);
		} finally {
			try {
				excelStream.close();
			} catch (IOException ex) {
				LOGGER.error(ex.getMessage(), ex);
			}
		}
		return arrayDatosGeneral;
	}

	private static String obtenerCellValidada(XSSFWorkbook workbook, Cell cell) {

		DataFormatter dataFormatter = new DataFormatter();
		String valor = "";

		if (cell != null) {
			switch (cell.getCellTypeEnum()) {
			case STRING:
				valor = dataFormatter.formatCellValue(cell);
				break;
			case FORMULA:
				valor = String.valueOf((int) cell.getNumericCellValue());
				break;
			case BLANK:
				valor = dataFormatter.formatCellValue(cell);
				break;
			default:
				if(DateUtil.isCellDateFormatted(cell)){
			        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
			        valor =  sdf.format(cell.getDateCellValue());
			    } else {
			    	valor = dataFormatter.formatCellValue(cell);
			    }
				break;
			}
		}
		return valor;
	}

	@SuppressWarnings("rawtypes")
	public static boolean setFieldGeneric(Object targetObject, String fieldName, Object fieldValue) {
		Field field;
		try {
			String fieldNameTemp = getValMethodAttribute(targetObject, fieldName);
			if (fieldNameTemp != null)
				fieldName = fieldNameTemp;

			field = targetObject.getClass().getDeclaredField(fieldName);
		} catch (NoSuchFieldException e) {
			field = null;
		}
		Class superClass = targetObject.getClass().getSuperclass();
		while (field == null && superClass != null) {
			try {
				field = superClass.getDeclaredField(fieldName);
			} catch (NoSuchFieldException e) {
				superClass = superClass.getSuperclass();
			}
		}
		if (field == null) {
			return false;
		}
		field.setAccessible(true);
		try {
			field.set(targetObject, fieldValue);
			return true;
		} catch (IllegalAccessException e) {
			LOGGER.error(e.getMessage(), e);
			return false;
		}
	}

	private static String getValMethodAttribute(Object objeto, String parametro) {
		String valor = null;
		try {
			Method[] metodos = objeto.getClass().getMethods();
			for (Method method : metodos) {
				if (method.getName().equals("getAttribute")) {
					valor = (String) method.invoke(objeto, parametro);
				}
			}
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return valor;

	}

	@SuppressWarnings("unchecked")
	public static byte[] updateDropDownXLSX(InputStream excelStream, Map<String, Object> mapaListas) {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		try (XSSFWorkbook hssfWorkbook = new XSSFWorkbook(excelStream)) {
			// Representación del más alto nivel de la hoja excel.
			// Elegimos la hoja que se pasa por parámetro.
			XSSFSheet hssfSheet = hssfWorkbook.getSheetAt(1);
			// Objeto que nos permite leer un fila de la hoja excel, y de aquí extraer el
			// contenido de las celdas.

			for (Map.Entry<String, Object> entry : mapaListas.entrySet()) {
				List<Generico> lista = (List<Generico>) entry.getValue();

				int indexBody = 1;
				for (Generico generico : lista) {
					Row rowBody = hssfSheet.createRow(indexBody);
					Cell celdaCodigo = rowBody.createCell(0);
					celdaCodigo.setCellValue(generico.getId());

					Cell celdaDescripcion = rowBody.createCell(1);
					celdaDescripcion.setCellValue(generico.getDescripcion());
					indexBody++;
				}
			}
			hssfWorkbook.setForceFormulaRecalculation(true);
			hssfWorkbook.write(os);

		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			try {
				excelStream.close();
			} catch (IOException ex) {
				LOGGER.error(ex.getMessage(), ex);
			}
		}
		return os.toByteArray();
	}

	public String addObservacionesXLSXBase64(InputStream excelStream, List<?> lista, boolean deleteFileOK) {
		byte[] arrayBytes = addObservacionesXLSX(excelStream, lista, deleteFileOK);
		String fileBase64 = "";
		if (arrayBytes != null) {
			fileBase64 = Base64.getEncoder().encodeToString(arrayBytes);
		}
		return fileBase64;
	}

	public byte[] addObservacionesXLSX(InputStream excelStream, List<?> lista, boolean deleteFileOK) {
		// Representación del más alto nivel de la hoja excel.
		try (XSSFWorkbook workbook = new XSSFWorkbook(excelStream);
				ByteArrayOutputStream os = new ByteArrayOutputStream()) {

			// Elegimos la hoja que se pasa por parámetro.
			XSSFSheet sheet = workbook.getSheetAt(0);
			// Objeto que nos permite leer un fila de la hoja excel, y de aquí extraer el
			// contenido de las celdas.
			Row xssRow = sheet.getRow(sheet.getTopRow());

			DataFormatter dataFormatter = new DataFormatter();

			int indiceObservacion = xssRow.getLastCellNum() - 1;

			/** CREACION DE CABECERA OBSERVACION SI NO EXISTIERA */
			Cell cabeceraObs = xssRow.getCell(indiceObservacion);
			String valorCabecera = dataFormatter.formatCellValue(cabeceraObs);
			if (!Constantes.OBSERVACION.equalsIgnoreCase(valorCabecera)) {
				indiceObservacion = indiceObservacion + 1;
				Cell cellNew = xssRow.createCell(indiceObservacion);
				cellNew.setCellType(CellType.STRING);
				cellNew.setCellValue("OBSERVACION");
			}

			/** AGREGAR LA OBSERVACION A LA FILA */
			for (int i = 0; i < lista.size(); i++) {
				Field field = lista.get(i).getClass().getDeclaredField("observacionResultado");
				field.setAccessible(true);// NOSONAR
				String valor = (String) field.get(lista.get(i));

				Cell cellNew = sheet.getRow(i + 1).createCell(indiceObservacion);
				cellNew.setCellType(CellType.STRING);
				cellNew.setCellValue(Constantes.VACIO);

				if (!StringUtils.isEmpty(valor)) {
					cellNew.setCellValue(valor);
				}
			}

//			if (deleteFileOK) {
//
//				int cantidadFilas = sheet.getLastRowNum();
//				/** ELIMINACION DE FILAS EXCEL */
//				for (int i = cantidadFilas; i >= 0; i--) {
//					Row row = sheet.getRow(i);
//					Cell cell = row.getCell(indiceObservacion);
//					String valor = dataFormatter.formatCellValue(cell);
//					if (StringUtils.isEmpty(valor)) {
//						sheet.removeRow(row);
//					}
//				}
//			}

			workbook.write(os);
			byte[] bytes = os.toByteArray();

			workbook.close();

			return bytes;

		} catch (FileNotFoundException fileNotFoundException) {
			LOGGER.error(fileNotFoundException.getMessage(), fileNotFoundException);
		} catch (IOException ex) {
			LOGGER.error(ex.getMessage(), ex);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			try {
				excelStream.close();
			} catch (IOException ex) {
				LOGGER.error(ex.getMessage(), ex);
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public static byte[] updateCodigoDescrDropDownXLSX(InputStream excelStream, Map<String, Object> mapaListas,
			Map<String, Object> mapaListasDependientes) {

		try (XSSFWorkbook hssfWorkbook = new XSSFWorkbook(excelStream)) {
			// Representación del más alto nivel de la hoja excel.
			// Elegimos la hoja que se pasa por parámetro.
			XSSFSheet hssfSheet = hssfWorkbook.getSheetAt(Constantes.HOJA_EXCEL_UNO);
			// Objeto que nos permite leer un fila de la hoja excel, y de aquí extraer el
			// contenido de las celdas.

			Row rowHeader = hssfSheet.getRow(0);
			if (rowHeader == null)
				rowHeader = hssfSheet.createRow(0);

			int indexHeader = 0;

			for (Map.Entry<String, Object> entry : mapaListas.entrySet()) {
				String key = entry.getKey();
				List<Generico> lista = (List<Generico>) entry.getValue();

				Cell cellHeaderCod = rowHeader.getCell(indexHeader);
				if (cellHeaderCod == null)
					cellHeaderCod = rowHeader.createCell(indexHeader);
				cellHeaderCod.setCellValue("COD " + key);

				Cell cellHeaderDescrip = rowHeader.getCell(indexHeader + 1);
				if (cellHeaderDescrip == null)
					cellHeaderDescrip = rowHeader.createCell(indexHeader + 1);
				cellHeaderDescrip.setCellValue(key);

				int indexBody = 1;
				for (Generico generico : lista) {
					Row rowBody = hssfSheet.getRow(indexBody);
					if (rowBody == null)
						rowBody = hssfSheet.createRow(indexBody);

					Cell cellBody = rowBody.createCell(indexHeader);
					cellBody.setCellValue(generico.getId());

					Cell cellBodyDescrip = rowBody.createCell(indexHeader + 1);
					cellBodyDescrip.setCellValue(generico.getDescripcion());

					indexBody++;
				}
				indexHeader += 2;
			}

			//
			List<Generico> listaOrganigrama = (List<Generico>) mapaListasDependientes.get("SIGLA UO");
			List<ComboPuesto> listaPuestos = (List<ComboPuesto>) mapaListasDependientes.get("PUESTO");

			Cell cellHeaderCod = rowHeader.getCell(indexHeader);
			if (cellHeaderCod == null)
				cellHeaderCod = rowHeader.createCell(indexHeader);
			cellHeaderCod.setCellValue("COD SIGLA UO");

			Cell cellHeaderDescrip = rowHeader.getCell(indexHeader + 1);
			if (cellHeaderDescrip == null)
				cellHeaderDescrip = rowHeader.createCell(indexHeader + 1);
			cellHeaderDescrip.setCellValue("SIGLA UO");

			Cell cellHeaderCodPuesto = rowHeader.getCell(indexHeader + 2);
			if (cellHeaderCodPuesto == null)
				cellHeaderCodPuesto = rowHeader.createCell(indexHeader + 2);
			cellHeaderCodPuesto.setCellValue("COD PUESTO");

			Cell cellHeaderDescripPuesto = rowHeader.getCell(indexHeader + 3);
			if (cellHeaderDescripPuesto == null)
				cellHeaderDescripPuesto = rowHeader.createCell(indexHeader + 2);
			cellHeaderDescripPuesto.setCellValue("PUESTO");

			int indexBody = 1;
			for (Generico generico : listaOrganigrama) {

				long cantPuestos = listaPuestos.stream()
						.filter(g -> g.getOrganigramaId().equals((long) generico.getId())).count();

				if (cantPuestos > 0) {
					for (ComboPuesto comboPuesto : listaPuestos) {
						if (comboPuesto.getOrganigramaId() == (long) generico.getId()) {
							Row rowBodyCod = hssfSheet.getRow(indexBody);
							if (rowBodyCod == null)
								rowBodyCod = hssfSheet.createRow(indexBody);

							Cell cellBody = rowBodyCod.createCell(indexHeader);
							cellBody.setCellValue(generico.getId());

							Cell cellBodyDesc = rowBodyCod.createCell(indexHeader + 1);
							cellBodyDesc.setCellValue(generico.getDescripcion());

							Cell cellBodyCodPuesto = rowBodyCod.createCell(indexHeader + 2);
							cellBodyCodPuesto.setCellValue(comboPuesto.getId());

							Cell cellBodyDescPuesto = rowBodyCod.createCell(indexHeader + 3);
							cellBodyDescPuesto.setCellValue(comboPuesto.getDescripcion());

							indexBody++;
						}
					}
				} else {
					Row rowBodyCod = hssfSheet.getRow(indexBody);
					if (rowBodyCod == null)
						rowBodyCod = hssfSheet.createRow(indexBody);

					Cell cellBody = rowBodyCod.createCell(indexHeader);
					cellBody.setCellValue(generico.getId());

					Cell cellBodyDesc = rowBodyCod.createCell(indexHeader + 1);
					cellBodyDesc.setCellValue(generico.getDescripcion());

					Cell cellBodyCodPuesto = rowBodyCod.createCell(indexHeader + 2);
					cellBodyCodPuesto.setCellValue(Constantes.VACIO);

					Cell cellBodyDescPuesto = rowBodyCod.createCell(indexHeader + 3);
					cellBodyDescPuesto.setCellValue(Constantes.VACIO);

					indexBody++;
				}
			}

			int indexHeaderUnicoUO = 14;
			int indexHeaderUnicoUODescrip = 15;
			Cell cellHeaderCodSiglaUnico = rowHeader.getCell(indexHeaderUnicoUO);
			if (cellHeaderCodSiglaUnico == null)
				cellHeaderCodSiglaUnico = rowHeader.createCell(indexHeaderUnicoUO);
			cellHeaderCodSiglaUnico.setCellValue("COD SIGLA UNICO");

			Cell cellHeaderSiglaUnico = rowHeader.getCell(indexHeaderUnicoUODescrip);
			if (cellHeaderSiglaUnico == null)
				cellHeaderSiglaUnico = rowHeader.createCell(indexHeaderUnicoUODescrip);
			cellHeaderSiglaUnico.setCellValue("SIGLA UNICO");

			indexBody = 1;
			for (Generico generico : listaOrganigrama) {
				Row rowBodyCod = hssfSheet.getRow(indexBody);
				if (rowBodyCod == null)
					rowBodyCod = hssfSheet.createRow(indexBody);

				Cell cellBody = rowBodyCod.createCell(indexHeaderUnicoUO);
				cellBody.setCellValue(generico.getId());

				Cell cellBodyDescrip = rowBodyCod.createCell(indexHeaderUnicoUODescrip);
				cellBodyDescrip.setCellValue(generico.getDescripcion());
				indexBody++;
			}

			int indexHeaderPuesto = 16;
			int indexHeaderPuestoDescrip = 17;

			Cell cellHeaderPuesto = rowHeader.getCell(indexHeaderPuesto);
			if (cellHeaderPuesto == null)
				cellHeaderPuesto = rowHeader.createCell(indexHeaderPuesto);
			cellHeaderPuesto.setCellValue("COD PUESTO");

			Cell cellHeaderPuestoDescr = rowHeader.getCell(indexHeaderPuestoDescrip);
			if (cellHeaderPuestoDescr == null)
				cellHeaderPuestoDescr = rowHeader.createCell(indexHeaderPuestoDescrip);
			cellHeaderPuestoDescr.setCellValue("PUESTO");

			indexBody = 1;
			for (ComboPuesto comboPuesto : listaPuestos) {
				Row rowBodyCod = hssfSheet.getRow(indexBody);
				if (rowBodyCod == null)
					rowBodyCod = hssfSheet.createRow(indexBody);

				Cell cellBody = rowBodyCod.createCell(indexHeaderPuesto);
				cellBody.setCellValue(comboPuesto.getId());

				Cell cellBodyDescrip = rowBodyCod.createCell(indexHeaderPuestoDescrip);
				cellBodyDescrip.setCellValue(comboPuesto.getDescripcion());
				indexBody++;
			}

			hssfWorkbook.setForceFormulaRecalculation(true);

			ByteArrayOutputStream os = new ByteArrayOutputStream();
			hssfWorkbook.write(os);
			byte[] bytes = os.toByteArray();

			hssfWorkbook.close();
			return bytes;

		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			try {
				excelStream.close();
			} catch (IOException ex) {
				LOGGER.error(ex.getMessage(), ex);
			}
		}
		return null;

	}
}
