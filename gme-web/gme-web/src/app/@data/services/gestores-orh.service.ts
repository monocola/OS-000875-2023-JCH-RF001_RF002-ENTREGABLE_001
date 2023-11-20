import { Injectable } from '@angular/core';
import { GestoresOrhRepository } from '../../@domain/repository/gestores-orh.repository';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs/Observable';
import { GestoresOrh } from '../model/gestores-orh';
import { Const } from './const';
import { map } from 'rxjs/operators';


@Injectable({
  providedIn: 'root'
})
export class GestoresOrhService implements GestoresOrhRepository {

  constructor(
    private http: HttpClient,
  ) { }

  getParametros(value: string): Observable<any[]> {
    throw new Error('Method not implemented.' + value);
  }

  getList(entidadId: number): Observable<GestoresOrh[]> {
    let url = `${Const.API_ENTIDAD}v1/gestores/orh/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        let listaGestoresOrh: GestoresOrh[];
        if (response.status.success) {
          listaGestoresOrh = response.payload.gestores;
        }

        return listaGestoresOrh?.map(item => {
          item.disabledNotification = item.estadoRegistro !== '1';
          item.disabledDelete = item.estadoRegistro === '2';
          if (!item.disabledNotification) {
            item.estado = "Activo";
          } else {
            item.estado = "Inactivo";
          }
          item.colorEstado = '<span class="' + item.estado.toLowerCase() +
            '"><span class="dot"></span>&nbsp&nbsp' + item.estado.toUpperCase() + '</span>';
          return item;
        });
      })
    );
  }

  setRegistrarGestor(body: any): Observable<any> {
    body.rolId = Const.GESTOR_ID;
    let url = `${Const.API_ENTIDAD}v1/gestores/orh`;
    return this.http.post(url, body).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]);
        }
      }),
    );
  }

  deleteGestorOrh(gestorId: number): Observable<GestoresOrh> {
    const url = `${Const.API_ENTIDAD}v1/gestorOrh/${gestorId}`;
    return this.http.delete(url).pipe(
      map((response: any) => {
        return response.status.success;
      })
    );
  }

  getGestorId(gestorId: number): Observable<any> {
    let url = `${Const.API_ENTIDAD}v1/gestores/orhById/${gestorId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  saveGestorORH(body: any): Observable<any> {
    let url = `${Const.API_ENTIDAD}v1/gestores/orh`;
console.info(body);
    return this.http.put(url, body).pipe(
      map((response: any) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response;
        }
      }),
    );
  }

}


