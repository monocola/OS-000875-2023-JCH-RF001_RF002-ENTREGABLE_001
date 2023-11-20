import { Observable } from "rxjs";

export abstract class OrganigramaRepository {
    abstract getPeopleAdmin(value: string): Observable<any[]>;
    abstract searchOrganigramas(body: any): Observable<any[]>;
    abstract setOrganoOrUnidadFromChart(organo: any);
    abstract uploadFileMasivo(body: any): Observable<any>;
    abstract downloadExcel(): Observable<string>;
    abstract getGestionOrganigramaByFiltro(body: any): Observable<any[]>;
    abstract registerOrUpdateUO(body: any, organigramaId?: number): Observable<boolean>;
    abstract deleteGO(organigramaId): Observable<any>;

    abstract validarSiglaUnicidad(mensaje: any, IdUO: number): Observable<any>;
    abstract validarDescripcionUnicidad(mensaje: any, IdUO: number): Observable<any>;

    abstract getOrganoStored();

    abstract getListPuesto(entidadId: number, personaId: number): Observable<any[]>;
    abstract getDatosPersonales(entidadId: number, personaId: number): Observable<any[]>;
    abstract updateDatosPersonales(body: any): Observable<any>;
}

