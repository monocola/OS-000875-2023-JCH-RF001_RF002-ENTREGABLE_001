import { Observable } from 'rxjs';

export abstract class EvaluacionesEntidadRepository {
  abstract asignarEvaluacionesEntidad(listaJerarquia: any[]): Observable<any>;
  abstract updateEvaluacionEntidad(evaluaciones, jerarquiaId): Observable<any>;
  abstract eliminarJerarquia(jerarquiaId): Observable<any>;
}
