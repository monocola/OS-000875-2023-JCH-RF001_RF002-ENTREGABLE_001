import { Observable } from 'rxjs/Observable';

export abstract class SolicitudRepository {

  abstract getRuc(ruc: string): Observable<any>;
  abstract getSolicitudExtId(Id: number): Observable<any>;

}
