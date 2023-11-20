import { Observable } from "rxjs";
import { EntidadGestor } from "src/app/@data/model/entidadGestor";
import { UsuarioGdr } from "src/app/@data/model/usuarioGdr";
import { ApiEntidad, ApiPlaniEntidad } from '../../@data/model/entidad';
import { ComboItem } from '../../@data/model/comboItem';
import { GenericResponse } from '../../@data/model/generic/generic.response';

export abstract class ImplementacionRepository {
    abstract getParametros(value: string): Observable<any[]>;
    abstract getListEntidades(): Observable<ApiEntidad[]>;
    abstract getListImplementacion(ciclo, nivelGob?, sector?, tipEntidad?: number, entidad?: string): Observable<ApiPlaniEntidad[]>;
    abstract getListadoEntidadRector(rectorId, flagSoloAsociadas?, nivelGob?, sector?, tipEntidad?: number, entidad?: string): Observable<EntidadGestor[]>;
    /* abstract getOrganoStored();
    abstract getListPuesto(entidadId: number, personaId: number): Observable<any[]>; */
    abstract getListUsuariosGdr(entidadId: number): Observable<UsuarioGdr[]>;
    abstract getOptionsMenuTemp(): Observable<boolean>;
    abstract getObtenerPDFs(uuId: String): any;
    abstract restoreMenu(): void;
    abstract validateTempMenu(): boolean;
    abstract getDetailGestorGDR(gestorEntidadID: number): Observable<UsuarioGdr>;
    abstract getTypeDocuments(): Observable<ComboItem[]>;
    abstract getServidoresCiviles(entidadId: number, tipoDocumentoId: number, nroDocumento: number): Observable<any>;
    abstract updateGestor(data: any, archivo: any): Observable<any>;
    abstract newGestor(data: any): Observable<GenericResponse<void>>;
}

