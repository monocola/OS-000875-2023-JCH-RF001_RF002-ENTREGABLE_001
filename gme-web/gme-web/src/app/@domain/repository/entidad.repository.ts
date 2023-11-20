import { Observable } from "rxjs";

export abstract class EntidadRepository {
    abstract subirDocumentoEntidad(body: any, archivo: any): Observable<any>;
    abstract actualizarEntidad(request: any): Observable<any>;
    // abstract getListarEntidad(): Observable<any>;
}

