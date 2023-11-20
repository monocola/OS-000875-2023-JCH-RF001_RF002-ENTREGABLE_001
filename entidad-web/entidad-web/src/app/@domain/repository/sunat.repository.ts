import { Observable } from "rxjs";

export abstract class SunatRepository {
    abstract getSunatInfo(ruc: string): Observable<any>;
}
