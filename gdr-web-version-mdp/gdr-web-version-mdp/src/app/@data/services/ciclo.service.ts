import { Injectable } from '@angular/core';
import { CicloRepository } from '../../@domain/repository/ciclo.repository';
import { Observable } from 'rxjs/Observable';
import { HttpClient } from '@angular/common/http';
import { Const } from './const';
import { catchError, map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Utils } from '../../utils/utils';
import { ToastService } from '../../@presentation/@common-components/toast';

@Injectable({
  providedIn: 'root',
})
export class CicloService implements CicloRepository {

  constructor(
    private http: HttpClient,
    private toast: ToastService,
  ) {}

  getListCiclo(entidadId: number): Observable<any[]> {
    const url = `${Const.API_PLANIFICACION}v1/ciclos/${entidadId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

  getPuesto(entidadId: number, puesto: string): Observable<any[]> {
    if ( puesto == null ) { puesto = ''; }
    const url = `${Const.API_ENTIDAD}v1/puesto/filtrar?entidadId=${entidadId}&descripcion=test`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaComboPuesto;
          }
        }
      )
    );
  }

  registraCiclo(
    entidadId: number,
/*     puestoTitularId: number,
    puestoJefeId: number,
    puestoGestor: number, */
    anio: number,
    fechaIni: Date,
    fechaFin: Date,
/*     descripcion: string,
    estadoCicloId: number, */
    estadoCicloDesc: string): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      status: {
        success: true,
        error: {
          code: null,
          httpCode: null,
          messages: []
        }
      },
      payload: {
        entidadId: entidadId,
     /*    titularPuestoId: puestoTitularId,
        jefePuestoId: puestoJefeId,
        gestorPuestoId: puestoGestor, */
      //  descripcion: descripcion,
        anio: anio,
        fechaIni: Utils.formatFechaString( fechaIni, 'DD/MM/YYYY' ),
        fechaFin: Utils.formatFechaString( fechaFin, 'DD/MM/YYYY'),
      //  estadoCicloId: estadoCicloId,
        estadoCicloDesc: estadoCicloDesc,
        estadoRegistro: '1'
      },
    };
    const url = `${Const.API_PLANIFICACION}v1/ciclos/apertura`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response;
          }
        }),
      catchError((err) => {
        this.toast.showToast('Ocurrió un error al registrar el Ciclo', 'danger', 'Atención');
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  getCiclosFilter(): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/ciclos/anios`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

  getCiclosFilterFormalizados(): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/ciclos/anios/formalizados`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }
}
