import { Observable } from "rxjs";

export abstract class ListaEvaluacionesRepository {

  abstract getListaEvaluaciones(): Observable<any>;

}
