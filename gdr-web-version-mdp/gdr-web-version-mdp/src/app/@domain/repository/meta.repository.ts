import { Observable } from "rxjs";

export abstract class MetaRepository {

    abstract updateMeta(data): Observable<boolean>;
    abstract saveMeta(data): Observable<boolean>;
    abstract detalleMeta(idMeta): Observable<any>;
    abstract estadosMeta(): Observable<{ id, value }[]>;
    abstract getDataEvaluador(): any;
    abstract getDataParticipante(): any;
    abstract getDataCiclo(): any;
    abstract updateEdicion(body: any): Observable<any>;
    abstract eliminarMeta(metaId: number): Observable<any>;



}
