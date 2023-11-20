import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root',
})
export class Const {
  public static API_FILE_SERVER: string;
  public static API_SEGURIDAD: string;
  public static API_PERSONA: string;
  public static API_MAESTRA: string;
  public static API_ENTIDAD: string;
  public static API_CONVOCATORIA: string;
  public static API_EVALUACION: string;
  public static API_SELECCION: string;
  public static API_REPORTE: string;
  public static API_POSTULANTE: string;

  public static USERNAME_SEGURIDAD: string;
  public static PASSWORD_SEGURIDAD: string;
  public static APPLICATION_ID: number;

  public static R_ADMIN_SERVIR: number;
  public static R_ADMIN_ENTIDAD: number;
  public static R_SUPER_ADMIN_ENTIDAD: number;
  
  public static R_GESTOR_ORH: number;
  public static R_COORDINADOR: number;
  public static R_CONFIGURADOR: number;
  public static R_POSTULANTE: number;

  public static MD_DL1041: string;
  public static MD_DL276: string;
  public static MD_DL728: string;
  public static MD_DL30057: string;
  public static MD_DL1057: string;

  public static M_NO_APLICA: string;
  public static T_NO_APLICA: string;

  public static MD_PRA_PRE_PROF: string;
  public static MD_PRA_PROF: string;

  public static EGRESADO: string;
  public static ESTUDIANTE: string;

  public static SIT_ACA_MAE: string;
  public static SIT_ACA_BAC: string;
  public static SIT_ACA_TIT_LIC: string;
  public static SIT_ACA_DOC: string;
  public static SIT_ACA_EGRE: string;
  public static SIT_ACA_NO_APLICA: string;

  public static DIPLOMADO: string;
  public static TALLER: string;
  public static CURSO: string;

  public static INF_BASE_LEGAL: string;
  public static INF_BONIFICACION: string;
  public static INF_DEC_JURADA: string;
  public static INF_CRI_EVALUACION: string;
  public static INF_BASE_LEGAL_ESP: string;

  public static ETA_BASE_PROCESO: string;
  public static ETA_BASE_APROBADO: string;
  public static ETA_BASE_COMPLETADO: string;
  public static ETA_BASE_OBSERVADO: string;
  public static ETA_BASE_ENVIADO: string;
  public static ETA_BASE_POR_REVISAR: string;
  public static ETA_BASE_REVISADO: string;
  public static ETA_BASE_PUBLICADO: string;
  public static ETA_BASE_POR_PUBLICAR: string;
  public static ETA_BASE_ELIMINADO: string;

  public static ENT_ESTADO_PENDIENTE: number;
  public static PER_TIPO_PERSONA_JUR: number;
  public static PER_TIPO_PERSONA_NAT: number;
  public static TIPO_ORGANO: number;
  public static TIPO_UNIDAD_ORGANICA: number;

  public static EST_COMUNI_PROCESO: string;
  public static EST_COMUNI_OBSERVADO: string;
  public static EST_COMUNI_POR_REVISAR: string;
  public static EST_COMUNI_APROBADO: string;
  public static EST_COMUNI_PUBLICADO: string;

  public static TIP_COMUNI_CRONOGRAMA: number;
  public static TIP_COMUNI_FE_DE_ERRATAS: number;
  public static TIP_COMUNI_DESIERTA: number;
  public static TIP_COMUNI_ACTA_RESULTADOS: number;
  public static TIP_COMUNI_CANCELADA: number;

  public static EST_EVAL_CALIF: string;
  public static EST_EVAL_NOCALIF: string;
  public static EST_EVAL_DESCALF: string;
  public static EST_EVAL_NOASIST: string;

  public static COD_CON_TEC: string;
  public static COD_CUR_ESP: string;
  public static COD_PRO_ESP: string;
  public static COD_CON_OFI: string;
  public static COD_CON_IDIO: string;
  public static EST_CONTRATO_CREADO: string;
  public static EST_CONTRATO_PENDIENTE: string;
  public static EST_CONTRATO_SUSCRITO: string;
  public static EST_CONTRATO_ANULADO: string;

  public static TIPO_ETAPA_CRONOGRAMA_DIFUSION: string;
  public static TIPO_ETAPA_CRONOGRAMA_RECLUTAMIENTO: string;
  public static TIPO_ETAPA_CRONOGRAMA_EVALUACION: string;
  public static TIPO_ETAPA_CRONOGRAMA_ELECCION: string;

  public static EST_CONV_PROC: string;
  public static EST_CONV_DES: string;
  public static EST_CONV_CANCEL: string;
  public static EST_CONV_CULM: string;
  public static EST_CONV_GENER: string;

  public static CRITERIO_EVALUACION: number;

  public static COD_CONOCIM_TECNICO: number;
  public static COD_CURSOS_ESPECIAL: number;
  public static COD_PROGRAM_ESPECIAL: number;
  public static COD_CONOCIM_OFIMATICA: number;
  public static COD_CONOCIM_IDIOMA: number;
  public static TIPO_FILES_PERF_REG_MAS: string[];
  public static EST_PERFILES_REVISADO: number;
  public static EST_PERFILES_POR_REVISAR: number;
  public static ORI_PERFILES_INDIVIDUAL: string;
  public static ORI_PERFILES_MASIVO: string;

  public static COD_PEF_IDI_BAS: number;
  public static COD_PEF_IDI_INT: number;
  public static COD_PEF_IDI_AVA: number;

  public static COD_PEF_OFI_BAS: number;
  public static COD_PEF_OFI_INT: number;
  public static COD_PEF_OFI_AVA: number;

  public static NVL_EDU_PRI: number;
  public static NVL_EDU_SEC: number;
  public static NVL_EDU_TEC_BAS: number;
  public static NVL_EDU_TEC_SUP: number;
  public static NVL_EDU_UNI: number;

  public static ACTIVO: string;
  public static INACTIVO: string;

  constructor(private http: HttpClient) {}

  public loadCommonConfig() {
    return this.http
      .get('./assets/config/common.config.json')
      .toPromise()
      .then((config: any) => {
        Const.API_FILE_SERVER = config.public_base_url_file_server;
        Const.API_SEGURIDAD = config.public_base_url_seguridad;
        Const.API_PERSONA = config.public_base_url_persona;
        Const.API_MAESTRA = config.public_base_url_maestra;
        Const.API_ENTIDAD = config.public_base_url_entidad;
        Const.API_CONVOCATORIA = config.public_base_url_convocatoria;
        Const.API_EVALUACION = config.public_base_url_evaluacion;
        Const.API_SELECCION = config.public_base_url_seleccion;
        Const.API_POSTULANTE = config.public_base_url_postulante;
        Const.API_REPORTE = config.public_base_url_reporte;
        Const.API_POSTULANTE = config.public_base_url_postulante;
      })
      .catch((err: any) => {
        console.error(err);
      });
  }

  public loadEntidadConfig() {
    return this.http
      .get('./assets/config/entidad-web.config.json')
      .toPromise()
      .then((config: any) => {
        Const.CRITERIO_EVALUACION = config.criterio_evaluacion;

        Const.EST_COMUNI_PROCESO = config.comunicado.proceso;
        Const.EST_COMUNI_OBSERVADO = config.comunicado.observado;
        Const.EST_COMUNI_POR_REVISAR = config.comunicado.por_revisar;
        Const.EST_COMUNI_APROBADO = config.comunicado.aprobado;
        Const.EST_COMUNI_PUBLICADO = config.comunicado.publicado;

        Const.TIP_COMUNI_CRONOGRAMA = config.tipo_comunicado.cronograma;
        Const.TIP_COMUNI_FE_DE_ERRATAS = config.tipo_comunicado.fe_de_erratas;
        Const.TIP_COMUNI_DESIERTA = config.tipo_comunicado.desierta;
        Const.TIP_COMUNI_ACTA_RESULTADOS =
          config.tipo_comunicado.acta_resultados;
        Const.TIP_COMUNI_CANCELADA = config.tipo_comunicado.cancelada;

        Const.EST_CONTRATO_CREADO = config.contrato.creado;
        Const.EST_CONTRATO_PENDIENTE = config.contrato.pendiente;
        Const.EST_CONTRATO_SUSCRITO = config.contrato.suscrito;
        Const.EST_CONTRATO_ANULADO = config.contrato.anulado;

        Const.ENT_ESTADO_PENDIENTE = config.ent_estado_pendiente;
        Const.PER_TIPO_PERSONA_JUR = config.tipo_persona_juridica;
        Const.PER_TIPO_PERSONA_NAT = config.tipo_persona_natural;
        Const.TIPO_ORGANO = config.tipo_organo;
        Const.TIPO_UNIDAD_ORGANICA = config.tipo_organo_unidad_organica;

        Const.USERNAME_SEGURIDAD = config.client_id;
        Const.PASSWORD_SEGURIDAD = config.client_secret;
        Const.APPLICATION_ID = config.aplicacion_id;

        Const.R_ADMIN_SERVIR = config.roles.admin_servir;
        Const.R_ADMIN_ENTIDAD = config.roles.admin_entidad;
        Const.R_SUPER_ADMIN_ENTIDAD = config.roles.super_admin_entidad;
        Const.R_GESTOR_ORH = config.roles.gestor_orh;
        Const.R_COORDINADOR = config.roles.coordinador;
        Const.R_CONFIGURADOR = config.roles.configurador;
        Const.R_POSTULANTE = config.roles.postulante;

        Const.MD_DL30057 = config.maestraDetalle.ley30057;
        Const.MD_DL1041 = config.maestraDetalle.dl1401;
        Const.MD_DL276 = config.maestraDetalle.dl276;
        Const.MD_DL728 = config.maestraDetalle.dl728;
        Const.MD_DL1057 = config.maestraDetalle.dl1057;

        Const.M_NO_APLICA = config.maestraDetalle.mod_no_aplica;
        Const.T_NO_APLICA = config.maestraDetalle.tipo_no_aplica;

        Const.EGRESADO = config.maestraDetalle.egresado;
        Const.ESTUDIANTE = config.maestraDetalle.estudiante;

        Const.MD_PRA_PRE_PROF = config.maestraDetalle.pract_preprofesional;
        Const.MD_PRA_PROF = config.maestraDetalle.pract_profesional;

        Const.SIT_ACA_MAE = config.maestraDetalle.sit_acad_maestria;
        Const.SIT_ACA_BAC = config.maestraDetalle.sit_acad_bachiller;
        Const.SIT_ACA_TIT_LIC = config.maestraDetalle.sit_acad_titulo_lic;
        Const.SIT_ACA_DOC = config.maestraDetalle.sit_acad_doctorado;
        Const.SIT_ACA_EGRE = config.maestraDetalle.sit_acad_egresado;
        Const.SIT_ACA_NO_APLICA = config.maestraDetalle.sit_acad_no_aplica;

        Const.DIPLOMADO = config.maestraDetalle.diplomado;
        Const.TALLER = config.maestraDetalle.taller;
        Const.CURSO = config.maestraDetalle.curso;

        Const.INF_BASE_LEGAL = config.informes.base_legal;
        Const.INF_BONIFICACION = config.informes.bonificaciones;
        Const.INF_DEC_JURADA = config.informes.declara_jurada;
        Const.INF_CRI_EVALUACION = config.informes.crit_evaluacion;
        Const.INF_BASE_LEGAL_ESP = config.informes.base_legal_esp;

        Const.ETA_BASE_PROCESO = config.etapa_base.proceso;
        Const.ETA_BASE_APROBADO = config.etapa_base.aprobado;
        Const.ETA_BASE_COMPLETADO = config.etapa_base.completado;
        Const.ETA_BASE_OBSERVADO = config.etapa_base.observado;
        Const.ETA_BASE_ENVIADO = config.etapa_base.enviado;

        Const.ETA_BASE_POR_REVISAR = config.etapa_base.por_revisar;
        Const.ETA_BASE_REVISADO = config.etapa_base.revisado;
        Const.ETA_BASE_PUBLICADO = config.etapa_base.publicado;
        Const.ETA_BASE_POR_PUBLICAR = config.etapa_base.por_publicar;
        Const.ETA_BASE_ELIMINADO = config.etapa_base.eliminado;

        Const.COD_CON_TEC = config.maestraDetalle.cod_con_con_tec;
        Const.COD_CUR_ESP = config.maestraDetalle.cod_con_cur_esp;
        Const.COD_PRO_ESP = config.maestraDetalle.cod_con_pro_esp;
        Const.COD_CON_OFI = config.maestraDetalle.cod_con_con_ofi;
        Const.COD_CON_IDIO = config.maestraDetalle.cod_con_con_idio;

        Const.EST_EVAL_CALIF = config.etapa_evaluacion.califica;
        Const.EST_EVAL_NOCALIF = config.etapa_evaluacion.no_califica;
        Const.EST_EVAL_DESCALF = config.etapa_evaluacion.descalificado;
        Const.EST_EVAL_NOASIST = config.etapa_evaluacion.no_asistio;

        Const.EST_CONV_PROC = config.etapa_convocatoria.en_proceso;
        Const.EST_CONV_DES = config.etapa_convocatoria.desierta;
        Const.EST_CONV_CANCEL = config.etapa_convocatoria.cancelada;
        Const.EST_CONV_CULM = config.etapa_convocatoria.culminada;
        Const.EST_CONV_GENER = config.etapa_convocatoria.generado;

        Const.TIPO_ETAPA_CRONOGRAMA_DIFUSION = config.etapa_cronograma.difusion;
        Const.TIPO_ETAPA_CRONOGRAMA_RECLUTAMIENTO =
          config.etapa_cronograma.reclutamiento;
        Const.TIPO_ETAPA_CRONOGRAMA_EVALUACION =
          config.etapa_cronograma.evaluacion;
        Const.TIPO_ETAPA_CRONOGRAMA_ELECCION = config.etapa_cronograma.eleccion;

        Const.COD_CONOCIM_TECNICO =
          config.maestra_tipo_conocimiento.cod_conocimientos_tecnicos;
        Const.COD_CURSOS_ESPECIAL =
          config.maestra_tipo_conocimiento.cod_cursos_especializacion;
        Const.COD_PROGRAM_ESPECIAL =
          config.maestra_tipo_conocimiento.cod_programas_especializacion;
        Const.COD_CONOCIM_OFIMATICA =
          config.maestra_tipo_conocimiento.cod_conocimientos_ofimatica;
        Const.COD_CONOCIM_IDIOMA =
          config.maestra_tipo_conocimiento.cod_conocimientos_idioma;
        Const.TIPO_FILES_PERF_REG_MAS = config.tipo_files_perf_reg_mas;

        Const.EST_PERFILES_REVISADO = config.perfiles_estado_revisado.revisado;
        Const.EST_PERFILES_POR_REVISAR =
          config.perfiles_estado_revisado.por_revisar;
        Const.ORI_PERFILES_INDIVIDUAL = config.perfiles_origen.individual;
        Const.ORI_PERFILES_MASIVO = config.perfiles_origen.masivo;
        Const.COD_CONOCIM_TECNICO =
          config.maestra_tipo_conocimiento.cod_conocimientos_tecnicos;
        Const.COD_CURSOS_ESPECIAL =
          config.maestra_tipo_conocimiento.cod_cursos_especializacion;
        Const.COD_PROGRAM_ESPECIAL =
          config.maestra_tipo_conocimiento.cod_programas_especializacion;
        Const.COD_CONOCIM_OFIMATICA =
          config.maestra_tipo_conocimiento.cod_conocimientos_ofimatica;
        Const.COD_CONOCIM_IDIOMA =
          config.maestra_tipo_conocimiento.cod_conocimientos_idioma;

        Const.COD_PEF_IDI_BAS = config.perfil_idiomas.basico;
        Const.COD_PEF_IDI_INT = config.perfil_idiomas.intermedio;
        Const.COD_PEF_IDI_AVA = config.perfil_idiomas.avanzado;

        Const.COD_PEF_OFI_BAS = config.perfil_office.basico;
        Const.COD_PEF_OFI_INT = config.perfil_office.intermedio;
        Const.COD_PEF_OFI_AVA = config.perfil_office.avanzado;

        Const.NVL_EDU_PRI = config.maestraDetalle.nvl_edu_pri;
        Const.NVL_EDU_SEC = config.maestraDetalle.nvl_edu_sec;
        Const.NVL_EDU_TEC_BAS = config.maestraDetalle.nvl_edu_tec_bas;
        Const.NVL_EDU_TEC_SUP = config.maestraDetalle.nvl_edu_tec_sup;
        Const.NVL_EDU_UNI = config.maestraDetalle.nvl_edu_uni;

        Const.ACTIVO = config.activo;
        Const.INACTIVO = config.inactivo;
      })
      .catch((err: any) => {
        console.error(err);
      });
  }
}
