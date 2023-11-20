import { HttpEvent } from '@angular/common/http';
import { Observable } from 'rxjs';

export abstract class EvaluacionConocimientosRepository {
  abstract listarCategorias(
    cantidad: number,
    categoria: string
  ): Observable<any>;
  abstract deleteCategoria(categoriaId: number): Observable<any>;
  abstract deleteExamen(examenId: number): Observable<any>;
  abstract listarExamenes(
    cantidad: number,
    examen: string,
    convocatoriaId: number,
    perfilId: number
  ): Observable<any>;
  abstract listarEvaluaciones(cantidad: number): Observable<any>;
  abstract comboConvocatorias(): Observable<any>;
  abstract comboPerfiles(convocatoriaId: number): Observable<any>;
  abstract guardarCategoria(
    categoriaId: number,
    descripcion: string
  ): Observable<any>;
  abstract buscarBandejaCategoria(
    cantidad: number,
    descripcion: string
  ): Observable<any>;
  abstract listarTipoDuraciones(): Observable<any>;
  abstract guardarActualizarPregunta(
    idPregunta: number,
    idDuracion: number,
    idCategoria: number,
    descPregunta: string,
    imagenUrl: string,
    tipoPregunta: number,
    explicacionOpc: string,
    alternativas: any,
    puntos: number,
    rangoIncrementeal: number
  ): Observable<any>;
  abstract obtenerDetallePregunta(idPregunta: number): Observable<any>;
  abstract fileUpload(
    fileBase64: string,
    fileName: string,
    extension: string,
    observacion: string,
    path: string
  ): Observable<any>;
  abstract listarPreguntas(
    categoria: number,
    descripcion: string,
    tipo: string
  ): Observable<any>;
  abstract listarTipoPregunta(): Observable<any>;
  abstract deletePreguntaDeCategoria(idPregunta: number): Observable<any>;
  abstract listarPreguntasPorCategoria(
    categoriaId: number,
    idExamen: number
  ): Observable<any>;
  abstract maxCantidadRespuestas(): Observable<any>;
  abstract guardarActualizarExamen(
    idExamen: number,
    nombreExamen: string
  ): Observable<any>;
  abstract getDetalleExamen(examenId: number): Observable<any>;
  abstract guardarActualizarDetalleExamen(payload: any): Observable<any>;
  abstract eliminarExamenDetalle(examenDetalleId: any): Observable<any>;
  abstract listarModalidadEvaluacion(): Observable<any>;
  abstract listarExamenesProgramacion(): Observable<any>;
  abstract listarProgramaciones(
    convocatoria: number,
    examen: number,
    modalidad: number,
    perfil: number,
    grupo: number
  ): Observable<any>;
  abstract eliminarProgramacionByConvocatoria(
    convocatoriaId: number,
    perfilId: number
  ): Observable<any>;
  abstract listarGruposByConvocatoriaPerfil(
    convocatoriaId: number,
    perfilId: number
  ): Observable<any>;
  abstract eliminarGrupo(programacionId: any): Observable<any>;
  abstract generarExamenPdf(idProgramacion: number): Observable<any>;
  abstract downloadTemplateExcel(categoriaId: number): Observable<any>;
  abstract listarSedes(): Observable<any>;
  abstract getAllPersonasCuentaEntidad(idRol: number): Observable<any>;
  abstract registrarProgramacion(
    evaluadorId: number,
    modalidadId: number,
    convocatoriaId: number,
    cantidadPostulantes: number,
    nombreGrupo: string,
    fechaInicioExamen: string,
    fechaFinExamen: string,
    linkEvaluacion: string,
    examenId: number,
    sedeId: number,
    indicacionesAdicionales: string,
    perfilId: number,
    tipoExamen: string,
    programacionId: number,
    nombreSede: string,
    ubigeoId: string,
    lugarEvaluacion: string,
    referenciaLugar: string
  ): Observable<any>;
  abstract examenVirtualCabecera(programacionId: number): Observable<any>;
  abstract PreguntasByExamen(examenId: number): Observable<any>;
  abstract ObtenerAplicacionPorAbreviatura(
    abreviatura: string
  ): Observable<any>;
  abstract ObtenerRolesPorAplicacion(aplicacionId: number, nombreRol: string);

  abstract validaPlantillaPreguntasRespuestas(multipartData: FormData, categoriaId: number): Observable<any>;
  abstract guardarExamenPreguntasRespuestasMasiva(categoriaId: number, request: any): Observable<any>;
  abstract eliminarPreguntasExcel(cabeceraId: number): Observable<any>;
  abstract envioCorreo(
    convocatoriaId: number,
    perfilId: number
  ): Observable<any>;
  abstract buscarSeguimientoEvaluacion(
    tipoEvaluacion: number,
    baseId: number,
    filtros: any
  ): Observable<any>;
  abstract listarFechasEtapa(etapaId: number, baseId: number): Observable<any>;
  abstract getPlantillaPreguntasMasivas(): Observable<any>;
  abstract validaPlantillaPreguntas(multipartData: FormData, entidadId: number): Observable<any>;
  abstract guardarExamenPreguntasMasiva(entidadId: number, request: any): Observable<any>;
  abstract actualizarFlagSanciones(baseId: number, perfilId: number ): Observable<any>;
}
