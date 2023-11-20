import { Observable } from "rxjs";

export abstract class PuestoRepository {
    abstract registerOrUpdate(body: any, puestoId?: number): Observable<any>;
    abstract deleteGO(id: number): Observable<boolean>;
    abstract searchPuestos(body: any): Observable<any[]>;
    abstract getListPuesto(): Observable<any[]>;
    abstract downloadFormatPuesto(): Observable<any>;
    abstract uploadFileMasivo(body: any): Observable<any>;
}

