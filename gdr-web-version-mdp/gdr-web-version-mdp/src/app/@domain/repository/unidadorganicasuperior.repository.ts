import { Observable } from 'rxjs/Observable';
import { UnidadOrganicaCombo } from '../../@data/model/UnidadOrganicaCombo';

export abstract class UnidadOrganicaSuperiorRepository {
  abstract getUnidadOrganicaSuperior(tipo: string): Observable<UnidadOrganicaCombo[]>;

}
