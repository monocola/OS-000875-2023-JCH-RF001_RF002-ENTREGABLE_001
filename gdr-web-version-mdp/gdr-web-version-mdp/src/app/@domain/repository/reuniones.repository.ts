import { Observable } from "rxjs";
import { Reuniones } from "src/app/@data/model/reuniones";

export abstract class ReunionesRepository {
    abstract getPuestosTabs(): Observable<any>;
    abstract getPuestosPorEvaluadoTabs(): Observable<any>;
    abstract listReuniones(body: any): Observable<any>;
    abstract listReunionesEvaluado(body: any): Observable<any>;
    abstract listHistorialReuniones(body: any): Observable<any>;
    abstract listHistorialReunionesEvaluados(body: any): Observable<any>;
    abstract deleteReunion(reunionId: any): Observable<boolean>;
    abstract cancelarReunion(reunionId: any): Observable<boolean>;
    abstract agregarReunion(datos: Reuniones): Observable<any>;
    abstract notificarReunion(datos: Reuniones): Observable<any>;
    abstract editarReunion(datos: Reuniones): Observable<any>;
}
