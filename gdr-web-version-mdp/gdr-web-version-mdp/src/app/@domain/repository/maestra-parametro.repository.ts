import { Observable } from 'rxjs/Observable';
import { MaestraParametro } from '../../@data/model/maestra-parametro';

export abstract class MaestraParametroRepository {
  abstract getMaestraParametro(tipo_parametro: string): Observable<MaestraParametro[]>;

}
