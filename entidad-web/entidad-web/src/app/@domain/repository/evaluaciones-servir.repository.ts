import { Observable } from "rxjs";

export abstract class EvaluacionesServirRepository {

    abstract getEvaluaciones(): Observable<any[]>;
    abstract getRegimenesServir(params: any): Observable<any[]>;
    abstract getRegimenesCabecera(params: any): Observable<any[]>;
    abstract getModalidadesServir(params: any): Observable<any[]>;
    abstract getTiposServir(params: any): Observable<any[]>;
    abstract getEvaluacionesServir(params: any): Observable<any[]>;

    abstract saveEnlaceRegimen(listaJerarquia): Observable<any>;
    abstract editEnlaceRegimen(listaJerarquia, jerarquiaId): Observable<any>;
    abstract searchEnlacesRegimen(body): Observable<any>;
    abstract deleteJerarquia(nivel1: number, nivel2?: number, nivel3?: number): Observable<any>;

}
