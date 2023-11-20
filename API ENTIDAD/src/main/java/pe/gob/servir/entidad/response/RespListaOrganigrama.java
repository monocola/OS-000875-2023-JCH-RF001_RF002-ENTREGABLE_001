package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RespListaOrganigrama {
	private List<OrganigramaPadre> listaOrganigrama;

	@Getter
	@Setter
	public static class OrganigramaPadre {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijo> listaOrganigramaHijo;

		public OrganigramaPadre() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijo {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijoA> listaOrganigramaHijoA;

		public OrganigramaHijo() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoA {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijoB> listaOrganigramaHijoB;

		public OrganigramaHijoA() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoB {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijoC> listaOrganigramaHijoC;

		public OrganigramaHijoB() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoC {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijoD> listaOrganigramaHijoD;

		public OrganigramaHijoC() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoD {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijoE> listaOrganigramaHijoE;

		public OrganigramaHijoD() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoE {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;
		private List<OrganigramaHijoF> listaOrganigramaHijoF;

		public OrganigramaHijoE() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoF {
		private Long idOrganigrama;
		private Long idEntidad;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private Long orden;
		private String descripcion;
		private Long nivelId;
		private String desNivel;
		private String sigla;
		private Long naturalezaId;
		private String desNaturaleza;
		private String estadoId;
		private String estado;
		private Long padreId;
		private Long tipoOrganoId;
		private String desTipoOrgano;
		private Long nivelGobiernoId;
		private String descripcionCorta;
		private Long personaResponsableId;
		private String nombres;
		private String apellidoPaterno;
		private String apellidoMaterno;
		private Long tipoDocumentoId;
		private String tipoDocumento;
		private String numeroDocumento;
		private Long telefonoId;
		private String telefono;
		private Long correoId;
		private String correo;
		private Long paisId;
		private String nombrePais;
		private Long padreIdHijo;

		public OrganigramaHijoF() {
			super();
		}
	}
}
