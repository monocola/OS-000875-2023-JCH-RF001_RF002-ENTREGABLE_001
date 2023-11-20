package pe.gob.servir.entidad.util;

//import org.apache.commons.fileupload.disk.DiskFileItem;
//import org.apache.poi.util.IOUtils;
import org.apache.commons.fileupload.disk.DiskFileItem;
import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.apache.tomcat.util.http.fileupload.disk.DiskFileItemFactory;
import org.jboss.logging.Logger;
//import org.springframework.web.multipart.MultipartFile;
//import org.springframework.web.multipart.commons.CommonsMultipartFile;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;
import pe.gob.servir.entidad.common.Constantes;
//import org.apache.commons.fileupload.disk.DiskFileItemFactory;

import javax.xml.bind.DatatypeConverter;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Calendar;
import java.util.Date;

public class FilesUtil {

	private static final Logger LOGGER = Logger.getLogger(FilesUtil.class);
	
	public static String generarNombreArchivo(String nombre, String extension) {

		Calendar now = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat("ddMMyyHHmmssSSS");
		return nombre + sdf.format(now.getTime()) + "." + extension;
		
	}
	
	public static String generarNombreArchivoSinExt(String nombre) {

		SimpleDateFormat sdf = new SimpleDateFormat("ddMMyyHHmmssSSS");
		Calendar now = Calendar.getInstance();
		return nombre + sdf.format(now.getTime());
		
	}
	
	public static String verificarExtensionImagen(String base64){
		String extension = "";
		if(base64.contains(Constantes.TIPO_JPEG)){
			extension = Constantes.EXTENSION_JPG;
		}else if(base64.contains(Constantes.TIPO_JPG)){
			extension = Constantes.EXTENSION_JPG;
		}else if(base64.contains(Constantes.TIPO_PNG)){
			extension = Constantes.EXTENSION_PNG;
		}
		return extension;
	}
	
	public static String onlyName(String nameFile){
		int index = nameFile.lastIndexOf('.');
		return nameFile.substring(0, index);
	}
	public static String extension(String nameFile){
		int index = nameFile.lastIndexOf('.');
		return nameFile.substring(index + 1);
	}

	public static byte[] logo(String imagenString){
		byte[] bytes = null;
		if(imagenString != null){
			bytes = DatatypeConverter.parseBase64Binary(imagenString);
		}
		return bytes;
		
	}

	private static boolean verificarCrearDirectorio(String ruta) {
		File directorio = new File(ruta);
		if (!directorio.exists()) {
			return directorio.mkdirs();
		}
		return true;
	}

		
	public static InputStream  base64ToInputStream(String base64){
		byte[] bytes = DatatypeConverter.parseBase64Binary(base64);
		InputStream myInputStream = new ByteArrayInputStream(bytes); 
		return myInputStream;
	}
	
	
	
	public static String encodingBase64File(String filePath){
		String encodedString = null;
		try {		
		 byte[] fileContent = readFileToByteArray(new File(filePath)) ;
		 encodedString = Base64.getEncoder().encodeToString(fileContent);
		} catch (Exception e) {
			// TODO: handle exception
		}
		return encodedString;
	}
	
   
	
	 private static byte[] readFileToByteArray(File file){
	        // Creating a byte array using the length of the file
	        // file.length returns long which is cast to int
	        byte[] bArray = new byte[(int) file.length()];
	        try(FileInputStream fis = new FileInputStream(file)){
	            int byterLeidos = fis.read(bArray);
	            LOGGER.info("byter leidos "+byterLeidos);
	            fis.close(); 
	        }catch(IOException ioExp){
	            LOGGER.error(ioExp.getMessage(), ioExp);
	        }
	        return bArray;
	 }
	 
	 public static File readByteArrayToFile(byte[] arrayByte ,String ruta ){	
		    File file = new File(ruta);		
	        try (OutputStream os = new FileOutputStream(file)){
	            os.write(arrayByte);
	            System.out.println("Write bytes to file.");
	            os.close();	            
	            return file;
	        } catch (Exception e) {
	        	LOGGER.error(e.getMessage(), e);
	        }
	        return file;
	 }
	 

	 public static File base64ToFile(String base64){
			byte[] bytes = null;
			if(base64 != null){
				bytes = DatatypeConverter.parseBase64Binary(base64);
			}
			return readByteArrayToFile(bytes ,"temp");			
	 }

	 
		public static String formatDateToString(Date fecha) {
			SimpleDateFormat formato = new SimpleDateFormat(Constantes.FORMATO_FECHA_DD_MM_YYYY);
			return formato.format(fecha);
		}

	public static MultipartFile convertirBase64ToMultipartFile(String base64) throws IOException {
		// split data type from base64 string and obtain only base64 encoded string
		String[] base64Components = base64.split(",");
		if (base64Components.length > 1) {
			base64 = base64Components[1];
		}

		// convert base64 string to byte[]
		byte[] data = Base64.getDecoder().decode(base64);

		// create an instance of InputStream using byte[]
		InputStream inputStream = new ByteArrayInputStream(data);

		// create an instance of DiskFileItem
		DiskFileItem fileItem = (DiskFileItem) new DiskFileItemFactory().createItem("file", "text/plain", true, "file");

		// copy InputStream to DiskFileItem
		try (InputStream in = inputStream; OutputStream out = fileItem.getOutputStream()) {
			IOUtils.copy(in, out);
		} catch (Exception e) {
			throw new IOException("Invalid base64 content", e);
		}

		// get MultipartFile from DiskFileItem
		MultipartFile multipartFile = new CommonsMultipartFile(fileItem);

		return multipartFile;
	}

	 
}
