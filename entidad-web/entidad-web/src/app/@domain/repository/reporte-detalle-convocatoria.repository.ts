import { Observable } from "rxjs";

export abstract class ReporteDetalleConvocatoriaRepository {

  abstract getListaDetalleConvocatoria(convocatoriaId: number): Observable<any>;

  abstract getPerfilPuestoDetallePorConvocatoria(baseId: number): Observable<any>;

  abstract getVacantesEnEtapaActual(baseId: number): Observable<any>;


}
