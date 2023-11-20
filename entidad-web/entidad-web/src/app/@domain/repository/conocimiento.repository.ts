import { Observable } from 'rxjs';
import { Conocimiento } from '../../@data/model/conocimiento';

export abstract class ConocimientoRepository {
  abstract getConocimiento(
    tipoConocimiento?: number,
    categoria?: number,
    descripcion?: string
  ): Observable<any>;

  abstract deleteConocimiento(
    maeConocimientoId: number
  ): Observable<any>;

  abstract insertConocimiento(
    body: Conocimiento
  ): Observable<any>;

  abstract updateConocimiento(
    body: Conocimiento,
    maeConocimientoId: number
  ): Observable<any>;
}
