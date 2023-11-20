import { Organo } from 'src/app/@data/model/organo';
import { Observable } from 'rxjs';

export abstract class UnidadOrganicaRepository {
  abstract createOrUpdateUnidad(
    body: any,
    flagEdit?: boolean,
    idOrgano?: number,
    organo?: Organo
  ): Observable<any>;
  abstract getUnidadesOrganicas(toUpdateValues: boolean): Observable<any>;
  abstract deleteUnidad(id: number): Observable<Organo[]>;
  abstract downloadExcel(): Observable<string>;
  abstract uploadFileMasivo(body: any): Observable<any>;
}
