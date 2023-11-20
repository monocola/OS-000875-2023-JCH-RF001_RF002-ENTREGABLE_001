import { Observable } from 'rxjs';
import { Organo } from 'src/app/@data/model/organo';

export abstract class OrganoRepository {
  abstract searchOrganos(query: string): Observable<Organo[]>;
  abstract getOrganos(toUpdateValues: boolean): Observable<Organo[]>;
  abstract createOrUpdateOrgano(
    body: any,
    flagEdit?: boolean,
    idOrgano?: number,
    organo?: Organo
  ): Observable<any>;
  abstract deleteOrgano(id: number): Observable<Organo[]>;
  abstract downloadExcel(): Observable<string>;
}
