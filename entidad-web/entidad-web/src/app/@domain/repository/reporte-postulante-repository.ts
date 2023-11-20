import { Observable } from "rxjs";

export abstract class ReportePostulanteRepository {

    abstract buscarReportePostulantes(payload: any): Observable<any>;

}
