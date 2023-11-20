import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { MetaRepository } from '../../@domain/repository/meta.repository';
import { GenericRequest } from '../model/generic/generic.request';
import { GenericResponse } from '../model/generic/generic.response';
import { ResponseRequest, Status } from '../model/reponse-request';


@Injectable({
  providedIn: 'root',
})
export class MetaService implements MetaRepository {

  constructor(
    private http: HttpClient,
  ) { }

  updateMeta(data): Observable<boolean> {
    const url = `${Const.API_PLANIFICACION}v1/meta`;
    let body = new GenericRequest(data);
    return this.http.put<GenericResponse<void>>(url, body).pipe(
      map(response => {
        return response.status.success;
      })
    );
  }

  saveMeta(data): Observable<boolean> {
    const url = `${Const.API_PLANIFICACION}v1/meta`;
    let body = new GenericRequest(data);
    return this.http.post<GenericResponse<void>>(url, body).pipe(
      map(response => {
        return response.status.success;
      })
    );
  }
  detalleMeta(idMeta): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/meta/${idMeta}`;
    return this.http.get<GenericResponse<any>>(url).pipe(
      map(response => {
        return response.payload;
      })
    );
  }
  estadosMeta(): Observable<{ id, value }[]> {
    const url = `${Const.API_MAESTRA}v1/tiposparametro/ESTADO_META_GDR/parametros`;
    return this.http.get<GenericResponse<{ count, items: any[] }>>(url).pipe(
      map(response => {
        return response.payload.items;
      }), map (data => {
        return data.map(estado => {
          return {
            id: estado.codigoNumero,
            value: estado.valorTexto,
          };
        });
      })
    );
  }
  getDataEvaluador(): any {
    return JSON.parse(sessionStorage.getItem('selected_evaluador'));
  }
  getDataParticipante(): any {
    return JSON.parse(sessionStorage.getItem('selected_participante'));
  }
  getDataCiclo(): any {
    return JSON.parse(sessionStorage.getItem('ciclo'));
  }


  updateEdicion(tramaEnvio: any): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/meta/habilitar`;
    return this.http.put(url, tramaEnvio).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response;
          }
        }
      )
    );
  }


  eliminarMeta(metaId: number): Observable<any> {
    // const url = `${Const.API_ENTIDAD}v1/servidorCivil/${metaId}?estado=0`;
    const url = `${Const.API_PLANIFICACION}v1/meta/eliminar/${metaId}`;
    return this.http.put(url, {}).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }
}
