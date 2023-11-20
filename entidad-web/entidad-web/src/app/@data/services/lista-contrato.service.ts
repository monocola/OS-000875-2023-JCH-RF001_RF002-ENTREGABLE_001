import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ListaContratoRepository } from 'src/app/@domain/repository/lista-contrato.repository';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';


@Injectable({
  providedIn: 'root'
})
export class ListaContratoService extends ListaContratoRepository {

  constructor(private http: HttpClient) {
    super();
  }

  getContrato(
    idContrato: any,
  ): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/contratoconvenios?contratoId=${idContrato}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
  }


  GuardarContrato(
    idContrato: any,
    nroResolucion: string,
    nroInforme: string,
    fechaVinculacion: string,
    resolResponOrh: string,
    nroNorma: string,
    periodoPrueba: string,
    estadoId: number,
    nroMerito: number

  ): Observable<any> {
    const tramaEnvio = {

      trace: {
        traceId: "string",
      },
      payload: {
        idContrato: idContrato,
        nroResolucion: nroResolucion,
        nroInforme: nroInforme,
        fechaVinculacion: fechaVinculacion,
        resolResponOrh: resolResponOrh,
        nroNorma: nroNorma,
        periodoPrueba: periodoPrueba,
        estadoId: estadoId,
        nroPosPueMeri: nroMerito

      }
    };
    console.info(tramaEnvio);
    const url = `${Const.API_CONVOCATORIA}v1/contrato/${idContrato}`;

    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }


  GuardarConvenio(
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

  ): Observable<any> {
    const tramaEnvio = {

      trace: {
        traceId: "string",
      },
      payload: {
        idContrato: idContrato,
        direccionLabores: direccionLabores,
        periodoConvenio: periodoConvenio,
        fechaIniPractica: fechaIniPractica,
        fechaFinPractica: fechaFinPractica,
        horaIniPractica: horaIniPractica,
        horaFinPractica: horaFinPractica,
        puestoResponsableOrh: puestoResponsableOrh,
        responsableOrh: responsableOrh,
        tipoDocResponsable: tipoDocResponsable,
        nroDocResponsable: nroDocResponsable,
        puestoRepreUni: puestoRepreUni,
        nombRepreUni: nombRepreUni,
        tipoDocRepreUni: tipoDocRepreUni,
        nroDocRepreUni: nroDocRepreUni,
        direccionCentroEstudios: direccionCentroEstudios,
        rucCentroEstudios: rucCentroEstudios,
        estadoId: estadoId

      }
    };
    const url = `${Const.API_CONVOCATORIA}v1/convenio/${idContrato}`;

    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }


}
