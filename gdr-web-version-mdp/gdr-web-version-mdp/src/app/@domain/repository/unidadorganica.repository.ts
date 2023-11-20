import { Observable } from 'rxjs/Observable';
import { UnidadOrganicaCombo } from '../../@data/model/UnidadOrganicaCombo';

export abstract class UnidadOrganicaRepository {
  abstract getUnidadOrganica(tipo: string): Observable<UnidadOrganicaCombo[]>;

}
