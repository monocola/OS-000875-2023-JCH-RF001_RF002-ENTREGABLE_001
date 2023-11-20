import { Observable } from "rxjs";

export abstract class ReporteEvaluacionesRepository {

  abstract getListaEvaluaciones(baseId: number, entidadId: number): Observable<any>;

  abstract getCodigoConvocatoria(entidadId: number): Observable<any>;

  abstract getPerfilesPorConvocatoria(convocatoriaId: number): Observable<any>;

  abstract getListaNombresPorConvocatoriaPerfil(convocatoriaId: number, perfilId: number): Observable<any>;

  abstract getTipoDocumentos(): Observable<any>;

  abstract getIndicadoresPostulante(convocatoriaPostulId: number): Observable<any>;

  abstract getDataPuesto(baseId: number): Observable<any>;

  abstract buscarReporteEvaluaciones(payload: any): Observable<any>;

  abstract getinfoevaluacionpostulante(convocatoriaId: number, perfilId: number, postulanteId: number): Observable<any>;

  abstract getDescargaPDF(convocatoriaId: number, perfilId: number, postulanteId: number): Observable<any>;

}
