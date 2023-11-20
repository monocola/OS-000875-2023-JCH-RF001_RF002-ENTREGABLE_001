import { Observable } from 'rxjs';

export abstract class OrganigramaRepository {
  abstract getPeopleAdmin(value: string): Observable<any[]>;
  abstract searchOrganigramas(body: any): Observable<any[]>;
  abstract setOrganoOrUnidadFromChart(organo: any);
  abstract uploadFileMasivo(body: any): Observable<any>;
  abstract getOrganoStored();
}
