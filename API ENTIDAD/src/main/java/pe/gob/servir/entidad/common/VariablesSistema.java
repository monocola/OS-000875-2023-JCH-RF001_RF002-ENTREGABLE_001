package pe.gob.servir.entidad.common;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;

@Component
@Getter
@Setter
public class VariablesSistema {
	
	@Value("${correo.entidad.url.sistema}")
	public String urlSistema;
	
	@Value("${correo.entidad.url.pagina}")
	public String prefijoRedirecActSol;
	
	@Value("${correo.entidad.servir}")
	public String correoContacto;
	
	@Value("${rol.administrador.entidad.id}")
	public Long rolAdminEntidad;
	
	@Value("${persona.id.pais.peru}")
	public Long idPaisPeru;
	
	@Value("${seguridad.id.aplicacion.talento}")
	public Long aplicacionTalentoId;
	
	@Value("${ruta.archivo.excel.window.organo}")
	public String rutaExcelWindowOrgano; 
	
	@Value("${ruta.archivo.excel.linux.organo}")
	public String rutaExcelLinuxOrgano;
	
	@Value("${ruta.archivo.excel.window.unidad.organica}")
	public String rutaExcelWindowUnidOrganica; 
	
	@Value("${ruta.archivo.excel.linux.unidad.organica}")
	public String rutaExcelLinuxUnidOrganica;
	
	@Value("${ruta.file.server}")
	public String fileServer;
	
	@Value("${ruta.archivo.excel.window.organigrama}")
	public String rutaExcelWindowOrganigrama;
	
	@Value("${ruta.archivo.excel.linux.organigrama}")
	public String rutaExcelLinuxOrganigrama;
	
	@Value("${ruta.archivo.excel.window.servidor.civil}")
	public String rutaExcelWindowServidorCivil;
	
	@Value("${ruta.archivo.excel.linux.servidor.civil}")
	public String rutaExcelLinuxServidorCivil;
	
	@Value("${ruta.archivo.excel.window.puesto}")
	public String rutaExcelWindowPuesto;
	
	@Value("${ruta.archivo.excel.linux.puesto}")
	public String rutaExcelLinuxPuesto;
	
	@Value("${maestra.parametro.tipo.documento.ruc}")
	public int tipoDocumentoRuc;
	
	@Value("${maestra.parametro.tipo.documento.dni}")
	public int tipoDocumentoDni;
	
	@Value("${maestra.parametro.tipo.persona.juridica}")
	public int tipoPersonaJuridico;
	
	@Value("${maestra.parametro.tipo.persona.natural}")
	public int tipoPersonaNatural;
	
	@Value("${maestra.parametro.estado.solicitud.nuevo}")
	public int estadoSolicitudNuevo;
	
	@Value("${maestra.parametro.estado.solicitud.aprobado}")
	public int estadoSolicitudAprobado;
	
	@Value("${maestra.parametro.estado.solicitud.observado}")
	public int estadoSolicitudOserbado;
	
	@Value("${maestra.parametro.estado.solicitud.baja}")
	public int estadoBaja;
	
	@Value("${maestra.parametro.tipo.validacion.sunat}")
	public int tipoValidacionSunat;
	
	@Value("${maestra.parametro.tipo.validacion.reniec}")
	public int tipoValidacionReniec;
	
	@Value("${seguridad.grupo.entidad.admin}")
	public int grupoEntidadAdmin;

	@Value("${seguridad.perfil.gme.jefe.orh}")
	public int rolJefeOrhGme;
	
	@Value("${seguridad.perfil.gdr.jefe.orh}")
	public int rolJefeOrhGdr;
	
	@Value("${maestra.parametro.tipo.organo}")
	public int tipoOrgano;
	
	@Value("${maestra.parametro.tipo.unidad.organica}")
	public int tipoiUnidadOrganica;

	@Value("${link.gdr}")
	public String linkGdr;

	@Value("${correo.gdr}")
	public String correoGdr;
	
	@Value("${link.sgm}")
	public String linkSGM;
	
	@Value("${link.sgm.solicitud.editar}")
	public String linkSGMSolicitudEditar;

	@Value("${spring.rabbitmq.addresses}")
	public String rabbitAddress;

	@Value("${aes.secret.key}")
	private String secretKey;
	
}