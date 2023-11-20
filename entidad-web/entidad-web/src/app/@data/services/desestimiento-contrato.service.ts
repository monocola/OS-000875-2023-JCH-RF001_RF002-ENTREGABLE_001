import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';
import { DesestimientoRepository } from 'src/app/@domain/repository/desestimiento.repository';

@Injectable({
  providedIn: 'root'
})
export class DesestimientoContratoService implements DesestimientoRepository {

  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) { }

  DesestimarContrato(contratoId, categoriaDesestimientoId: number, motivoDesestimiento: string,
    estadoId: number, pdfBase64: string, tipoTrabajo: string,postulante:any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        contratoId: contratoId,
        categoriaDesestimientoId: categoriaDesestimientoId,
        motivoDesestimiento: motivoDesestimiento,
        estadoId: estadoId,
        pdfBase64: pdfBase64,
        tipoTrabajo: tipoTrabajo,
        postulante:postulante,
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/contrato/desestimar/${contratoId}`;
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

  SuscribirContrato(contratoId:number, estadoId: number, pdfBase64: string, fechaSuscripcion: string): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        estadoId: estadoId,
        pdfBase64: pdfBase64,
        fechaSuscripcion:fechaSuscripcion,
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/contrato/suscribir/${contratoId}`;
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

  DescargarContrato(contratoId:number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/contrato/download/${contratoId}`;
    return this.http.get(url).pipe(
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
