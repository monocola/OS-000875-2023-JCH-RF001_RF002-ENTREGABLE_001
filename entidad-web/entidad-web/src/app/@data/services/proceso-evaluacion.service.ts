import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Observable } from 'rxjs';
import { ProcesoEvaluacionRepository } from 'src/app/@domain/repository/proceso-evaluacion.repository';
import { Const } from './const';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { ToastService } from '../../@presentation/@common-components/toast';

@Injectable({
  providedIn: 'root',
})
export class ProcesoEvaluacionService extends ProcesoEvaluacionRepository {
  responseError = {
    trace: { traceId: null },
    status: {
      success: null,
      error: { code: '', httpCode: '', messages: null },
    },
    payload: null,
  };
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository,
    private toast: ToastService
  ) {
    super();
  }

  comboEstadoEvaCurricular(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/confEvaluacion/combo/tipEstadoEvaCurricular`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.maestraDetalles.filter(
            (m) => m.estadoRegistro !== '0'
          );
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  comboEstadoReqMinimos(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/confEvaluacion/combo/tipEstadoReqMin`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.maestraDetalles.filter(
            (m) => m.estadoRegistro !== '0'
          );
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarConfiguracionPerfil(
    nombrePerfil: string,
    regimenId: number,
    rolId: number,
    estadoRmId: number,
    estadoEcId: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_EVALUACION}v1/confEvaluacion/perfiles/buscar?entidadId=${entidadId}&nombrePerfil=${nombrePerfil}&regimenId=${regimenId}&rolId=${rolId}&estadoRmId=${estadoRmId}&estadoEcId=${estadoEcId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.perfiles;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
}
