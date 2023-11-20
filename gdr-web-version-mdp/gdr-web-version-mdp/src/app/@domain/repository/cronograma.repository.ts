import { Observable } from "rxjs";

export abstract class CronogramaRepository {

    abstract registerOrUpdateAct(body: any, flagReg?: boolean): Observable<boolean>;
    abstract getCronogramas(item: any, body: any): Observable<any[]>;
    abstract getResoluciones(item: any): Observable<any[]>;
    abstract getHistorialModificacionAct(body: any, id: number): Observable<any[]>;
    abstract deleteCronograma(organigramaId, actividadId): Observable<boolean>;
    abstract downloadExcel(organigramaId: number, anio: string): Observable<string>;
    abstract habilitaEdicion(cronogramaId: number, flagEdicion: string): Observable<any>;
    abstract getCboActividadesResolucion(cronogramaId: number, resolucionId: number): Observable<any>;
    abstract getCboActividades(cronogramaId: number): Observable<any>;

}
