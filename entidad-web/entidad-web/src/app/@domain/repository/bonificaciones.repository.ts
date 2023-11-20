import { Observable } from 'rxjs';
import { Bonificacion } from '../../@data/model/bonificaciones/bonificacion';

export abstract class BonificacionesRepository {
  abstract getBonificaciones(body): Observable<any>;
  abstract saveBonificacion(bonificacion: Bonificacion): Observable<boolean>;
  abstract updateBonificacion(bonificacion: any): Observable<boolean>;
  abstract deleteRegistro(bonificacionId: number): Observable<boolean>;
  abstract detail(bonificacionId: number): Observable<Bonificacion>;
}
