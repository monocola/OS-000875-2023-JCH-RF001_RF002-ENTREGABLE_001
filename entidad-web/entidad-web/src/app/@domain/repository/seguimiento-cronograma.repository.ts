import { Observable } from "rxjs";

export abstract class SeguimientoCronogramaRepository {

    abstract getCronograma(): Observable<any[]>;

}
