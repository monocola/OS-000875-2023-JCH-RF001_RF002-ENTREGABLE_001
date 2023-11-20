package pe.gob.servir.entidad.response;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RespListaOrgano {
	private List<OrganigramaPadre> listaOrganigrama;

	@Getter
	@Setter
	public static class OrganigramaPadre {
		private Long OrganigramaId;
		private Long EntidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijo;

		public OrganigramaPadre() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijo {
		private Long organigramaId;
		private Long entidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijoA;

		public OrganigramaHijo() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoA {
		private Long organigramaId;
		private Long entidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijoB;

		public OrganigramaHijoA() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoB {
		private Long organigramaId;
		private Long entidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijoC;

		public OrganigramaHijoB() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoC {
		private Long organigramaId;
		private Long EntidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijoD;

		public OrganigramaHijoC() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoD {
		private Long organigramaId;
		private Long entidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijoE;

		public OrganigramaHijoD() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoE {
		private Long organigramaId;
		private Long entidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
		private Integer cantidadHijoF;

		public OrganigramaHijoE() {
			super();
		}
	}

	@Getter
	@Setter
	public static class OrganigramaHijoF {
		private Long organigramaId;
		private Long entidadId;
		private Long areaId;
		private Long sedeId;
		private String puesto;
		private Long puestoId;
		private String urlFoto;
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
