import { Observable } from "rxjs";
import { MaestraParametro } from "src/app/@data/model/maestra-parametro";
import { PuestoUoServidorCivil } from "src/app/@data/model/puesto";
import { DatosPersonalesServidorCivil } from "src/app/@data/model/servidores-civiles";

export abstract class ServidoresRepository {
    /* abstract getPeopleAdmin(value: string): Observable<any[]>;

    abstract setOrganoOrUnidadFromChart(organo: any); */
    abstract uploadFileMasivo(body: any): Observable<any>;
    abstract downloadExcel(): Observable<string>;
    abstract getOrganigramas(): Observable<any[]>;
    abstract searchOrganigramaFilter(body: any): Observable<any[]>;
    abstract getTiposDocumento(): Observable<MaestraParametro[]>;

    abstract eliminarServidor(personaId: number): Observable<any>;
    abstract getDatosPersonales(detalleUoId: number, personaId: number): Observable<DatosPersonalesServidorCivil>;
    abstract agregarServidor(body: any): Observable<any>;
    abstract actualizarServidor(datos: DatosPersonalesServidorCivil): Observable<any>;
    abstract agregarPuesto(datos: PuestoUoServidorCivil): Observable<any>;
    abstract editarPuesto(datos: PuestoUoServidorCivil): Observable<any>;
    abstract getHisotrialPuestos(personaId: number, uoId: number): Observable<PuestoUoServidorCivil[]>;
    abstract listarPersonasParaPuesto(uoId: number, puestoId: number): Observable<any[]>;
    abstract getDatosPersonalesRegimen(detalleUoId: number, personaId: number, regimenId: number): Observable<DatosPersonalesServidorCivil>;

/*
    abstract getOrganoStored(); */


}

