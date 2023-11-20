package pe.gob.servir.entidad.util;

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.google.common.base.Strings;

import pe.gob.servir.entidad.api.dto.ApiPersonaRequestDTO;
import pe.gob.servir.entidad.api.dto.PersonaDTO;
import pe.gob.servir.entidad.common.Constantes;
import pe.gob.servir.entidad.model.ComboPuesto;
import pe.gob.servir.entidad.model.ComboUnidadOrganica;
import pe.gob.servir.entidad.model.Generico;
import pe.gob.servir.entidad.model.OrganigramaDTO;
import pe.gob.servir.entidad.model.PaisesDTO;
import pe.gob.servir.entidad.model.Parametro;
import pe.gob.servir.entidad.model.SolicitudPersona;
import pe.gob.servir.entidad.response.RespApiPersona;
import pe.gob.servir.entidad.response.RespBase;
import pe.gob.servir.entidad.response.RespParametro;

public class ParametrosUtil {

//	private static final Logger LOGGER = Logger.getLogger(ParametrosUtil.class);
	


	public static boolean ValidarKeyMapParametros(String llave, Map<String, Object> map) {
		boolean retorno = false;
		if (map.containsKey(llave)) {
			retorno = true;
		} else {
			retorno = false;
		}
		return retorno;
	}

	public static boolean isNumeric(String cadena) {
		boolean resultado;
		try {
			Long.parseLong(cadena);
			resultado = true;
		} catch (NumberFormatException excepcion) {
			resultado = false;
		}
		return resultado;
	}

	public static boolean isDouble(String cadena) {
		boolean resultado;
		try {
			Double.parseDouble(cadena);
			resultado = true;
		} catch (NumberFormatException excepcion) {
			resultado = false;
		}
		return resultado;
	}

	public static String validarParametroToken() {
		String rpta = Constantes.CONSTANTE_OK;
		String valor = "OK";
		if (valor != null && !valor.isEmpty()) {
			return rpta;
		} else {
			rpta = "Debe ingresar el token.";
		}
		return rpta;
	}

	public static String validarParametroNumericoValor(String valor) {

		String rpta = Constantes.CONSTANTE_OK;
		if (valor != null && !valor.isEmpty()) {
			if (!ParametrosUtil.isNumeric(valor)) {
				rpta = "El valor Ingresado debe ser Numerico.";
			}
		} else {
			rpta = "Debe ingresar el campo valor.";
		}
		return rpta;
	}

	public static boolean validarParametroDecimalValor(Double valor, Double valor2) {

		boolean rpta = true;
		if (valor > valor2) {
			rpta = false;
		}
		return rpta;
	}

	public static String validarParametroNumericoValor(String valor, String campo) {

		String rpta = Constantes.CONSTANTE_OK;
		if (valor != null && !valor.isEmpty()) {
			if (!ParametrosUtil.isNumeric(valor)) {
				rpta = "El campo " + campo + " debe ser Numerico.";
			}
		} else {
			rpta = "Debe ingresar el campo " + campo + ".";
		}
		return rpta;
	}

	public static boolean validarCantidadCarecteres(String valor, int longitudMinima) {
		try {
			boolean resp = (!StringUtils.isBlank(valor) && valor.length() >= longitudMinima);
			return resp;
		} catch (Exception e) {
		}
		return false;
	}

	public static boolean validarFechaInicioFin(Date fechaInicial, Date fechaFinal) {
		boolean retorno = true;
		if (fechaInicial.getTime() > fechaFinal.getTime())
			retorno = false;
		return retorno;
	}

	public static <T> RespBase<T> setearResponse(RespBase<T> response, Boolean status, String mensaje) {
		response.getStatus().setSuccess(status);
		response.getStatus().getError().getMessages().add(mensaje);
		return response;
	}
	
	public static <T> RespBase<T> setearResponse(RespBase<T> response, Boolean status, String info, String mensaje) {
		response.getStatus().setSuccess(status);
		response.getStatus().setInfo(info);
		response.getStatus().getError().getMessages().add(mensaje);
		return response;
	}

	public static <T> RespBase<T> setearListResponse(RespBase<T> response, Boolean status, String info, List<String> mensajes) {
		response.getStatus().setSuccess(status);
		response.getStatus().setInfo(info);
		response.getStatus().getError().getMessages().addAll(mensajes);
		return response;
	}
	
	public static Date ObtenerFechaActual() {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(new Date());
		return calendar.getTime();
	}

	public static String fechaHoraActualString() {
		Calendar fecha = new GregorianCalendar();
		String fechaActual = fecha.get(Calendar.YEAR) + "-" + formatear2Digitos((fecha.get(Calendar.MONTH) + 1)) + "-"
				+ formatear2Digitos(fecha.get(Calendar.DATE)) + " " + formatear2Digitos(fecha.get(Calendar.HOUR)) + ":"
				+ formatear2Digitos(fecha.get(Calendar.MINUTE)) + ":" + formatear2Digitos(fecha.get(Calendar.SECOND));
		return fechaActual;
	}

	public static String formatear2Digitos(int campo) {
		if (campo >= 0 && campo <= 9) {
			return "0" + campo;
		}
		return "" + campo;
	}

	public static String datePathReplaceRepositoryAlfresco(String pathAlfresco) {
		Date fecha = new Date();
		// SimpleDateFormat formatDia = new SimpleDateFormat("dd");
		SimpleDateFormat formatMes = new SimpleDateFormat("MM");
		SimpleDateFormat formatAnio = new SimpleDateFormat("yyyy");

		// String dia = formatDia.format(fecha);
		String mes = formatMes.format(fecha).toUpperCase();
		String anio = formatAnio.format(fecha);

		pathAlfresco = pathAlfresco.replace("{anio}", anio);
		pathAlfresco = pathAlfresco.replace("{mes}", mes);

		return pathAlfresco;
	}

	public static String generarNombreArchivo(String nombre, String extension) {

		Calendar now = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat("ddMMyyHHmmssSSS");
		String nombreArchivo = nombre + sdf.format(now.getTime()) + "." + extension;
		return nombreArchivo;

	}

	public static String onlyName(String nameFile) {
		int index = nameFile.lastIndexOf('.');
		return nameFile.substring(0, index);
	}

	public static String extension(String nameFile) {
		int index = nameFile.lastIndexOf('.');
		return nameFile.substring(index + 1);
	}

	public static void setearPersonaNatural(ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaNatural> apiPersona,
			PersonaDTO personaDto) {
		ApiPersonaRequestDTO.PersonaNatural persona = new ApiPersonaRequestDTO.PersonaNatural();
		persona.setNombres(personaDto.getNombres().toUpperCase());
		persona.setApellidoPaterno(personaDto.getApellidoPaterno().toUpperCase());
		persona.setApellidoMaterno(
				!Strings.isNullOrEmpty(personaDto.getApellidoMaterno()) ? personaDto.getApellidoMaterno().toUpperCase()
						: null);
		persona.setSexo(personaDto.getSexo());
		persona.setPaisId(personaDto.getPaisId());
		persona.setFechaNacimiento(personaDto.getFechaNacimiento() != null ?ParametrosUtil.StringToLocalDateDMY(personaDto.getFechaNacimiento()): null);
		apiPersona.setPersona(persona);

		apiPersona.getDocumentos().add(
				new ApiPersonaRequestDTO.Documentos(personaDto.getTipoDocumento(), personaDto.getNumeroDocumento()));
		if (!StringUtils.isEmpty(personaDto.getCorreoPrincipal())) {
			apiPersona.getCorreos().add(new ApiPersonaRequestDTO.Correo(personaDto.getCorreoPrincipalId(),
					Constantes.TIPO_CORREO_PRINCIPAL, personaDto.getCorreoPrincipal()));
		}
		if (!StringUtils.isEmpty(personaDto.getCorreoSecundario())) {
			apiPersona.getCorreos().add(new ApiPersonaRequestDTO.Correo(personaDto.getCorreoSecundarioId(),
					Constantes.TIPO_CORREO_ALTERNO, personaDto.getCorreoSecundario()));
		}
		if (personaDto.getUbigeoId() != null || !StringUtils.isEmpty(personaDto.getDireccionCompleta())) {
			apiPersona.getDirecciones().add(new ApiPersonaRequestDTO.Direccion(personaDto.getUbigeoId(), null,
					(!StringUtils.isEmpty(personaDto.getDireccionCompleta()) ? personaDto.getDireccionCompleta() : "-"),
					personaDto.getReferenciaDireccion()));
		}
		if (!StringUtils.isEmpty(personaDto.getRutaPaginaWeb())) {
			apiPersona.getWebs().add(new ApiPersonaRequestDTO.Web(personaDto.getRutaPaginaWeb()));
		}
		if (!StringUtils.isEmpty(personaDto.getTelefonoFijo())) {
			apiPersona.getTelefonos().add(new ApiPersonaRequestDTO.Telefono(personaDto.getTelefonoFijoId(),
					Constantes.TIPO_TELEFONO_CASA, null, personaDto.getTelefonoFijo(), personaDto.getAnexo()));
		}
		if (!StringUtils.isEmpty(personaDto.getCelularPrincipal())) {
			apiPersona.getTelefonos().add(new ApiPersonaRequestDTO.Telefono(personaDto.getCelularPrincipalId(),
					Constantes.TIPO_TELEFONO_CELULAR, null, personaDto.getCelularPrincipal(), null));
		}
	}

	public static void setearPersonaJuridica(ApiPersonaRequestDTO<ApiPersonaRequestDTO.PersonaJuridica> apiPersona,
			SolicitudPersona solicitudPersona,int TIPO_DOCUMENTO_RUC, RespBase<RespApiPersona> responsePer) {
	
		ApiPersonaRequestDTO.PersonaJuridica persona = new ApiPersonaRequestDTO.PersonaJuridica();
		persona.setRazonSocial(solicitudPersona.getRazonSocial().trim());
		persona.setNombreComercial(responsePer.getPayload().getPersonaJuridica().getNombreComercial());
		apiPersona.setPersona(persona);
		if (solicitudPersona.getUbigeoId() != null || !StringUtils.isEmpty(solicitudPersona.getDireccionCompleta())) {
			apiPersona.getDirecciones()
					.add(new ApiPersonaRequestDTO.Direccion(solicitudPersona.getUbigeoId(), null,
							(!StringUtils.isEmpty(solicitudPersona.getDireccionCompleta())
									? solicitudPersona.getDireccionCompleta()
									: "-"),
							solicitudPersona.getReferenciaDireccion()));
		}
		if (!StringUtils.isEmpty(solicitudPersona.getNumeroDocumento())) {
			apiPersona.getDocumentos().add(new ApiPersonaRequestDTO.Documentos(TIPO_DOCUMENTO_RUC,
					solicitudPersona.getNumeroDocumento()));
		}
		if (!StringUtils.isEmpty(solicitudPersona.getTelefonoFijo())) {
			apiPersona.getTelefonos().add(new ApiPersonaRequestDTO.Telefono(Constantes.TIPO_TELEFONO_CASA, null,
					solicitudPersona.getTelefonoFijo(), null));
		}
		if (!StringUtils.isEmpty(solicitudPersona.getCelularPrincipal())) {
			apiPersona.getTelefonos().add(new ApiPersonaRequestDTO.Telefono(Constantes.TIPO_TELEFONO_CELULAR, null,
					solicitudPersona.getCelularPrincipal(), null));
		}
		if (!StringUtils.isEmpty(solicitudPersona.getRutaPaginaWeb())) {
			apiPersona.getWebs().add(new ApiPersonaRequestDTO.Web(solicitudPersona.getRutaPaginaWeb()));
		}
		if (!StringUtils.isEmpty(solicitudPersona.getCorreoPrincipal())) {
			apiPersona.getCorreos().add(new ApiPersonaRequestDTO.Correo(null, Constantes.TIPO_CORREO_PRINCIPAL,
					solicitudPersona.getCorreoPrincipal()));
		}
	}

	public static String GeneradorCodigo() {
		Calendar now = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat("ddMMyyHHmmssSSS");
		return sdf.format(now.getTime());
	}

	public static boolean validarEmail(String valor) {
		return validar("^[\\w-\\.][A-Z]+\\@[\\w\\.-]+\\.[a-z]{2,4}$", valor);
	}

	private static boolean validar(String expReg, String valor) {
		Pattern patron = Pattern.compile(expReg);
		Matcher encajador = patron.matcher(valor);
		return encajador.matches();
	}

	public static RespBase<RespParametro> listaTipoDNI_CE(List<Parametro> lista) {
		List<Parametro> listaTipoDoc = new ArrayList<Parametro>();
		RespParametro listaFinal = new RespParametro();
		for (Parametro pa : lista) {
			if (pa.getValorTexto().equals("DNI") || pa.getValorTexto().equals("CE")) {
				listaTipoDoc.add(pa);
			}
		}
		listaFinal.setListaParametros(listaTipoDoc);
		return new RespBase<RespParametro>().ok(listaFinal);
	}

	public static List<Generico> convertirTipoDocumentoAGenerico(List<Parametro> lista) {
		List<Generico> listaGenerica = new ArrayList<>();
		if (lista != null) {
			for (Parametro item : lista) {
				listaGenerica.add(
						new Generico(item.getCodigoNumero(), item.getCodigoNumero().toString(), item.getValorTexto()));
			}
		}
		return listaGenerica;
	}

	public static List<Generico> convertirEstadoAGenerico(List<Parametro> lista) {
		List<Generico> listaGenerica = new ArrayList<>();
		if (lista != null) {
			for (Parametro item : lista) {
				listaGenerica.add(new Generico(item.getCodigoNumero(), item.getCodigoTexto(), item.getValorTexto()));
			}
		}
		return listaGenerica;
	}

	public static List<Generico> convertirNivelAGenerico(List<Parametro> lista) {
		return obtenerListaGenerica(lista);
	}

	private static List<Generico> obtenerListaGenerica(List<Parametro> lista) {
		List<Generico> listaGenerica = new ArrayList<>();
		if (lista != null) {
			for (Parametro item : lista) {
				listaGenerica.add(new Generico(item.getParametroId(), StringUtils.trimToEmpty(String.valueOf(item.getCodigoNumero()== null? Constantes.VACIO: item.getCodigoNumero())), StringUtils.trimToEmpty(item.getValorTexto())));
			}
		}
		return listaGenerica;
	}

	public static List<Generico> convertirNaturalezaAGenerico(List<Parametro> lista) {
		return obtenerListaGenerica(lista);
	}

	public static List<Generico> convertirOrganoAGenerico(List<OrganigramaDTO> lista) {
		List<Generico> listaGenerica = new ArrayList<Generico>();
		if (lista != null) {
			for (OrganigramaDTO item : lista) {
				listaGenerica
						.add(new Generico(item.getOrganigramaId().intValue(), item.getPuesto(), item.getDescripcion()));
			}
		}
		return listaGenerica;
	}

	public static List<Generico> convertirUnidadOrgAGenerico(List<ComboUnidadOrganica> lista) {
		List<Generico> listaGenerica = new ArrayList<>();
		if (lista != null) {
			for (ComboUnidadOrganica item : lista) {
				listaGenerica.add(new Generico(item.getId().intValue(), null, item.getSigla()));
			}
		}
		return listaGenerica;
	}
	public static List<Generico> convertirPuestosAGenerico(List<ComboPuesto> lista) {
		List<Generico> listaGenerica = new ArrayList<Generico>();
		if (lista != null) {
			for (ComboPuesto item : lista) {
				listaGenerica
						.add(new Generico(item.getId().intValue(), null, item.getDescripcion()));
			}
		}
		return listaGenerica;
	}
	
	public static List<Generico> convertirPaisesAGenerico(List<PaisesDTO> lista) {
		List<Generico> listaGenerica = new ArrayList<Generico>();
		if (lista != null) {
			for (PaisesDTO item : lista) {
				listaGenerica.add(new Generico(item.getPaisId(), item.getNacionalidad(), item.getNombrePais()));
			}
		}
		return listaGenerica;
	}

	public static <T> boolean listaNoVacia(List<T> lista) {
		boolean retorno = false;
		if (lista != null && lista.size() > 0) {
			retorno = true;
		} else {
			retorno = false;
		}
		return retorno;
	}
	
	public static boolean validarMaxNroCaracteres(String cadena, int longMax) {
		return cadena.length() <= longMax;
	}
	
	public static LocalDate StringToLocalDate(String date) {		
		DateTimeFormatter format = DateTimeFormatter.ofPattern("M/d/yy");
		return LocalDate.parse(date, format);
	}
	
	public static LocalDate StringToLocalDateDMY(String date) {	
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
		LocalDate localDate = LocalDate.parse(date, formatter);
		return localDate;
	}
	
	public static LocalDate StringToLocalDateFechaNacimiento(String date) {		
		DateTimeFormatter formatter = new DateTimeFormatterBuilder().appendPattern("M/d/")
	            .optionalStart()
	            .appendPattern("uuuu")
	            .optionalEnd()
	            .optionalStart()
	            .appendValueReduced(ChronoField.YEAR, 2, 2, 1920)
	            .optionalEnd()
	            .toFormatter();
		return LocalDate.parse(date, formatter);
	}

	public static boolean validarCorreo(String valor) {
		String  regex = "^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@"+ "[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$";
		Pattern patron = Pattern.compile(regex);
		Matcher encajador = patron.matcher(valor);
		return encajador.matches();
	}

	public static List<Generico> convertirNivelAGenericoRegimenLaboral(List<Parametro> listaParametros) {
		return obtenerListaGenericaRegimen(listaParametros);
	}

	private static List<Generico> obtenerListaGenericaRegimen(List<Parametro> listaParametros) {
			List<Generico> listaGenerica = new ArrayList<>();
			if (listaParametros != null) {
				for (Parametro item : listaParametros) {
					listaGenerica.add(new Generico( item.getCodigoNumero(),item.getParametroId().toString(), item.getValorTexto()));
				}
			}
			return listaGenerica;
	}
	public static String obtenerMensajeError( String mensaje) {
		//System.out.println("MENSAJEEEEEEEEEEE"+ mensaje);
		//List<String> obtenerMensaje = new ArrayList<>();
		//List<String> resultado = new ArrayList<>();
		//String mensajeError[] = mensaje.split(",");
		String[] parts = mensaje.split("messages");
		String part1 = parts[0];
		String part2 = parts[1];
		String finals = part2.substring(2, part2.length()-19);
		
		/*obtenerMensaje=Arrays.stream(mensajeError)
			   .filter(s -> s.contains("Nombres")).collect(Collectors.toList());
		if(!obtenerMensaje.isEmpty()) {
			resultado.add(obtenerMensaje.toString());
		}
		obtenerMensaje=Arrays.stream(mensajeError)
				   .filter(s -> s.contains("Apellido paterno")).collect(Collectors.toList());
		if(!obtenerMensaje.isEmpty()) {
			resultado.add(obtenerMensaje.toString());
		}
		obtenerMensaje=Arrays.stream(mensajeError)
				   .filter(s -> s.contains("Apellido materno")).collect(Collectors.toList());
		if(!obtenerMensaje.isEmpty()) {
			resultado.add(obtenerMensaje.toString());
		}
		obtenerMensaje=Arrays.stream(mensajeError)
				   .filter(s -> s.contains("Fecha de nacimiento")).collect(Collectors.toList());
		if(!obtenerMensaje.isEmpty()) {
			resultado.add(obtenerMensaje.toString());
		}
		obtenerMensaje=Arrays.stream(mensajeError)
				   .filter(s -> s.contains("correo")).collect(Collectors.toList());
		if(!obtenerMensaje.isEmpty()) {
			resultado.add(obtenerMensaje.toString());
		}
		
		for (String string : mensajeError) {
			if(string.equals("messages")){
				resultado.add(string);
			}
		}*/
		return finals.toString();
		
	}
}
