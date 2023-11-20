import { Observable } from "rxjs";

export abstract class ReportesRepository {

  abstract getCodigoConvocatoria(entidadId: number): Observable<any>;

  abstract getResumenConvocatoria(request: any): Observable<any>;

  abstract getRegimen(entidadId: number): Observable<any>;

  abstract getPostulantes(entidadId: number): Observable<any>;

  abstract getRoles(): Observable<any>;

  abstract getPerfilesEntidad(entidadId: number): Observable<any>;

  abstract getModalidadIngreso(entidadId: number, regimenId: number): Observable<any>;

  abstract getResponsables(entidadId: number, rolId: number): Observable<any>;

  abstract getEstadoConvocatoria(): Observable<any>;

  abstract getDescarga(request: any, baseId: number): Observable<any>;

  abstract getListaConvocatoria(request: any): Observable<any>;

  abstract getReporteServir(request: any): Observable<any>;
}
