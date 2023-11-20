import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { requestFilter } from 'src/app/utils/general';
import { ResponseRequest } from '../model/reponse-request';
import { BonificacionesRepository } from 'src/app/@domain/repository/bonificaciones.repository';
import { Bonificacion, DetailBonificacion, SaveBonificacion } from '../model/bonificaciones/bonificacion';
import { RequestGeneric } from '../model/generic/request.generic';
import { ResponseGeneric } from '../model/generic/response.generic';

@Injectable({
  providedIn: 'root',
})
export class BonificacionesService implements BonificacionesRepository {
  entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;

  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {}

  // Servicios - Repositorio

  getBonificaciones(body: any): Observable<any> {
    const params = requestFilter({
      tipoBonificacion: body.tipoBonificacion,
      titulo: body.titulo,
      estado: body.estado,
    });

    const url = `${Const.API_CONVOCATORIA}v1/bonificacion/filter?${params}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          response.payload.items.map((item) => {
            item.estado === '1'
              ? (item.estadoDescripcion = 'ACTIVO')
              : (item.estadoDescripcion = 'INACTIVO');
            return item;
          });
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveBonificacion(bonificacion: Bonificacion): Observable<boolean> {
    const url = `${Const.API_CONVOCATORIA}v1/bonificacion`;
    let body = new RequestGeneric<SaveBonificacion>(new SaveBonificacion(bonificacion));
    return this.http.post<ResponseGeneric<void>>(url, body, { observe: 'response' }).pipe(
      map(response => {
        if (response.body.status.success) {
          return true;
        } else {
          throw new Error(response.body.status.error.messages[0]);
        }
      })
    );
  }
  updateBonificacion(bonificacion: Bonificacion): Observable<boolean> {
    const url = `${Const.API_CONVOCATORIA}v1/bonificacion/${bonificacion.bonificacionId}`;
    let body = new RequestGeneric<SaveBonificacion>(new SaveBonificacion(bonificacion));
    return this.http.put<ResponseGeneric<void>>(url, body, { observe: 'response' }).pipe(
      map(response => {
        if (response.body.status.success) {
          return true;
        } else {
          throw new Error(response.body.status.error.messages[0]);
        }
      })
    );
  }

  deleteRegistro(bonificacionId: number): Observable<boolean> {
    const url = `${Const.API_CONVOCATORIA}v1/bonificacion/${bonificacionId}`;
    return this.http.delete<ResponseGeneric<void>>(url, { observe: 'response' }).pipe(
      map(response => {
        if (response.body.status.success) {
          return true;
        } else {
          throw new Error(response.body.status.error.messages[0]);
        }
      })
    );
  }

  detail(bonificacionId: number): Observable<Bonificacion> {
    const url = `${Const.API_CONVOCATORIA}v1/bonificacion/${bonificacionId}`;
    return this.http.get<ResponseGeneric<DetailBonificacion>>(url, { observe: 'response' }).pipe(
      map(response => {
        if (response.body.status.success) {
          return response.body.payload.bonificacion;
        } else {
          throw new Error(response.body.status.error.messages[0]);
        }
      }),
      map(data => {
        data.bonificacionDetalleDTOList = data.bonificacionDetalleDTOList.filter(item => item.estadoRegistro !== "0" );
        return data;
      }),
    );
  }
}
