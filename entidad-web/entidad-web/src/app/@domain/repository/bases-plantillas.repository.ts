import { Observable } from 'rxjs';

export abstract class BasesPlantillasRepository {
  abstract getPlantillasBase(body): Observable<any>;
  abstract saveOrUpdatePlantillaBase(body, informeId?): Observable<any>;
  abstract getBonificacionList(informeId): Observable<any>;
  abstract deleteBonificacionList(informeId): Observable<any>;
}
