import { Observable } from "rxjs";

export abstract class ListaContratoRepository {

    abstract getContrato(
        idContrato: any,
    ): Observable<any[]>;

    abstract GuardarContrato(
        idContrato: any,
        nroResolucion: string,
        nroInforme: string,
        fechaVinculacion: string,
        resolResponOrh: string,
        nroNorma: string,
        periodoPrueba: string,
        estadoId: number,
        nroPosPueMeri: number
    ): Observable<any>;

    abstract GuardarConvenio(
        idContrato: any,
        direccionLabores: string,
        periodoConvenio: string,
        fechaIniPractica: string,
        fechaFinPractica: string,
        horaIniPractica: string,
        horaFinPractica: string,
        puestoResponsableOrh: string,
        responsableOrh: string,
        tipoDocResponsable: string,
        nroDocResponsable: string,
        puestoRepreUni: string,
        nombRepreUni: string,
        tipoDocRepreUni: number,
        nroDocRepreUni: string,
        direccionCentroEstudios: string,
        rucCentroEstudios: string,
        estadoId: number
    ): Observable<any>;



}
