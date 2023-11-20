import { Observable } from "rxjs";
import { Sede } from "../../@data/model/sede";

export abstract class SedesRepository {
    abstract getSedesByFiltro(body: any): Observable<Sede[]>;
    abstract registerOrUpdateSede(body: any, sedeId?: number): Observable<boolean>;
    abstract deleteSede(idSede): Observable<boolean>;
}
