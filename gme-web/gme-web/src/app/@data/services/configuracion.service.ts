import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ConfiguracionRepository } from '../../@domain/repository/configuracion.repository';
import { Observable } from 'rxjs/Observable';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class ConfiguracionService implements ConfiguracionRepository {

  constructor(
    private http: HttpClient,
  ) {}

  notificaUsuarioReg(body: any): Observable<any> {
    let url = `${Const.API_NOTIFICACION}v1/enviarCorreo`;
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.payload.estado) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response;
        }
      }),
    );
  }

  enviaCorreoMasivo(body: any): Observable<any> {
    let url = `${Const.API_NOTIFICACION}v1/email`;
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.payload.estado) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response;
        }
      }),
    );
  }
}
