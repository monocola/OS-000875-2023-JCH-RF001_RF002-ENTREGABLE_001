import { Observable } from 'rxjs';
import { CboNivel } from 'src/app/@data/model/evaluacion-curricular/evaluacion';

export abstract class EvaluacionCurricularRepository {
  abstract getEstadosConvocatoria(): Observable<any>;
  abstract getNivelBonificacion(convocatoriaPostulante: number): Observable<CboNivel[]>;
  abstract getEvaluacionFiltro(entidadId: number, request: any): Observable<any>;
  abstract listarEstadoEvaluacionCurricular(request: any): Observable<any>;
  abstract listarConvocatoriasPostulante(request: any): Observable<any>;
  abstract listarPerfilPuestobyConvocatoria(idBase: number): Observable<any>;
  abstract experiencialLaboralByPerfil(perfilId: number, convocatoriaId: number): Observable<any>;
  abstract investigacionByPerfil(perfilId: number, convocatoriaId: number): Observable<any>;
  abstract listarPostulanteFormacionAcademica(convocatoriaId: number, postulanteId: number, perfilId: number): Observable<any>;
  abstract listarPostulanteExperiencia(convocatoriaId: number, postulanteId: number, perfilId: number): Observable<any>;
  abstract listarInvestigacionPostulante(convocatoriaId: number, postulanteId: number, perfilId: number): Observable<any>;
  abstract listarConvocatoriaCarreras(perfilId: number, convocatoriaId: number): Observable<any>;
  abstract listarConvocatoriaEspecialidades(perfilId: number, convocatoriaId: number): Observable<any>;
  abstract listarOtrosRequisitosPostulante(convocatoriaPostulanteId: number): Observable<any>;
  abstract listarBonoConvocatoria(convocatoriaPostulanteId: number): Observable<any>;
  abstract listarUrlDocumentos(convocatoriaPostulanteId: number): Observable<any>;
  abstract ListaConfigOtrosrequisitos(perfilId: number): Observable<any>;
  abstract listarDeclaracionesJuradas(convocatoriaId: number, postulanteId: number): Observable<any>;
  abstract documentoSustento(request: any): Observable<any>;
  abstract listarPuntajeEvaluacionCurricular(convocatoriaPostulanteId: number): Observable<any>;
  abstract actualizarSeccionEvaluacionFormac(seccionFormacionId: number, request: any): Observable<any>;
  abstract actualizarSeccionEvaluacionExp(seccionExperienciaId: number, request: any): Observable<any>;
  abstract evaluacionResumen(convocatoriaPostulanteId: number): Observable<any>;
  abstract getPDFEvaluacionCurricular(convocatoriaPostulanteId: number): Observable<any>;
  abstract getFlagTipoBono(resumenId: number, convocatoriaPostulanteId: number): Observable<any>;
  abstract guardarBonificacion(resumenId: number, item: any): Observable<any>;
  abstract setearBonificacion(resumenId: number, tipoBonificacion: number): Observable<any>;
  abstract actualizarRedereci(convocatoriaPostulante: number, request: any): Observable<any>;
  abstract listarEvaluacionInvestigacion(convocatoriaPostulante: number): Observable<any>;
  abstract actualizarEvaluacionInvestigacion(lstInvestigacionPostulante: any): Observable<any>;
}
