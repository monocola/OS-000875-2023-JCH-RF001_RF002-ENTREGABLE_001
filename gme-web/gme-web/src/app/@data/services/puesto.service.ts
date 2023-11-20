import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { PuestoRepository } from 'src/app/@domain/repository/puesto.repository';
import { requestFilterAll } from 'src/app/utils/general';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class PuestoService implements PuestoRepository {
  constructor(
    private http: HttpClient,
    private authService: AuthenticationRepository
  ) {}

  registerOrUpdate(body: any, puestoId?: number): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        puestoId: puestoId ? puestoId : null,
        entidadId: entidadId,
        organigramaId: body.organigramaId,
        descripcion: body.descripcion,
        esJefe: body.esJefe,
      },
    };
    if (puestoId) {
      let url_all = `${Const.API_ENTIDAD}v1/puesto/editar/${puestoId}`;
      return this.http.put(url_all, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return true;
          }
        })
      );
    } else {
      let url_all = `${Const.API_ENTIDAD}v1/puesto/registrar`;
      return this.http.post(url_all, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        })
      );
    }
  }

  searchPuestos(body: FiltroPuesto): Observable<any[]> {
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;

    const filterString = requestFilterAll(body);
    console.log(filterString);
    const url = `${Const.API_ENTIDAD}v1/puesto/listar?entidadId=${entidadId}&${filterString}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaPuesto;
      })
    );
  }

  getListPuesto(): Observable<any[]> {
    return this.http.get(`${Const.API_ENTIDAD}v1/puesto/listar`).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaPuestos;
        }
      })
    );
  }

  deleteGO(id: number): Observable<boolean> {
    return this.http
      .delete(`${Const.API_ENTIDAD}v1/puesto/eliminar/${id}?estado=0`)
      .pipe(
        map((response: ResponseRequest) => {
          if (response.status.success) {
            return true;
          } else {
            return false;
          }
        })
      );
  }

  downloadFormatPuesto() {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/puesto/downloadFormatPuesto?idEntidad=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  uploadFileMasivo(body: any): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/puesto/validaFormatoPuesto`; // TODO Cambiar cuando exista el de puestos
    const tramaEnvio = {
      value: body.split('base64,')[1],
      entidadId,
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((res: ResponseRequest) => {
        if (res.status.success) {
          if (res.payload.archivo.length === 0) {
            return true;
          } else {
            return res.payload;
          }
        } else {
          throw new Error(res.status.error.messages[0]).message;
        }
      })
    );
  }
}

interface FiltroPuesto {
  esJefe: string;
  unidadOrganicaID: number;
  nombrePuesto: string;
  puestoId: number;
}
