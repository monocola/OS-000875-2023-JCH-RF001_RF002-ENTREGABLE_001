import { Observable } from "rxjs";

export abstract class ReniecRepository {
    abstract getPersonInfo(document: string, typeDoc?: number): Observable<any>;
}
