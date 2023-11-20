import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ConfiguracionRepository } from '../../@domain/repository/configuracion.repository';
import { Observable } from 'rxjs/Observable';
import { UsuarioGestor } from '../model/usuariosGestores';
import { Const } from './const';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Injectable({
  providedIn: 'root',
})
export class ConfiguracionService implements ConfiguracionRepository {
  usuariosGestores: UsuarioGestor[] = [];

  constructor(
    private http: HttpClient,
    private authService: AuthenticationRepository,
    private toastService: ToastService,
  ) {}

  getUsuarioRector(): Observable<UsuarioGestor[]> {
    const url = `${Const.API_PLANIFICACION}v1/usuarioRector`;
    return this.http.get(url).pipe(
      map((response: any) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            this.usuariosGestores = response.payload.items;
            this.usuariosGestores.forEach( item => {
              item.nombreCompleto = item.nombres + ' ' + item.apellidoPaterno + ' ' + item.apellidoMaterno;
              item.conteoEntidad = item.nroEntidades.toString();
            });
            return this.usuariosGestores;

          }
        }
      )
    );
  }

  asignaRector(body: any): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/rectorasignado`;
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {

        console.log("response",response)
        console.log("response.status.error.messages[0]:",response.status.error.messages[0])
        console.log("response.status.error.messages[0]:",response.status.error.messages[0])

        if (response.status.success) {
          return response.payload;
        } else {
          this.toastService.showToast(Error(response.status.error.messages[0]).message, 'danger');
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
    );
  }
 
  /* estadoId?: number */
  getListUsuarios(): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    let url = `${Const.API_PLANIFICACION}v1/servidores/rectores?entidadId=${entidadId}`;
    /* if ( estadoId ) { url = url + `&estadoId=${estadoId}`; } */
    return this.http.get(url).pipe(
      map((response: any) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

    /* estadoId?: number */
    getListUsuariosServir(): Observable<any> {
      const entidadId = this.authService.getCurrentUserValue.entidadId;
      let url = `${Const.API_PLANIFICACION}v1/servidores/rectoresServir`;
      /* if ( estadoId ) { url = url + `&estadoId=${estadoId}`; } */
      return this.http.get(url).pipe(
        map((response: any) => {
 
            if (!response.status.success) {
              throw new Error(response.status.error.messages[0]).message;
            } else {
              return response.payload.items;
            }
          }
        )
      );
    }
 
  getCorreo(personaId: number): Observable<any> {
    let url = `${Const.API_PERSONA}v1/personas/${personaId}/correos`;
    return this.http.get(url).pipe(
      map((response: any) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

  notificaUsuarioReg(body: any): Observable<any> {
    let url = `${Const.API_NOTIFICACION}v1/enviarCorreo`;
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response;
        }
      }),
    );
  }

  asignaRectorEntidad(body: any): Observable<any> {
    let url = `${Const.API_PLANIFICACION}v1/asignacion/rectorEntidad`;
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response;
        }
      }),
    );
  }

}
