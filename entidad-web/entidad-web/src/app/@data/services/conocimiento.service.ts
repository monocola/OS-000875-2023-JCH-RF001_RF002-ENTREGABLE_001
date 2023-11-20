import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ConocimientoRepository } from 'src/app/@domain/repository/conocimiento.repository';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { Conocimiento } from '../model/conocimiento';

@Injectable({
  providedIn: 'root',
})
export class ConocimientoService extends ConocimientoRepository {
  responseError = {
    trace: { traceId: null },
    status: {
      success: null,
      error: { code: '', httpCode: '', messages: null },
    },
    payload: null,
  };
  constructor(private http: HttpClient ) {
    super();
  }
  getConocimiento(
    tipoConocimiento?: number,
    categoria?: number,
    descripcion?: string
  ): Observable<any> {
    let url = `${Const.API_CONVOCATORIA}v1/maestraConocimiento?`;
    if (tipoConocimiento) {
      url = url + `&tipoConocimiento=${tipoConocimiento}`;
    }
    if (categoria) {
      url = url + `&categoriaConocimiento=${categoria}`;
    }
    if (descripcion) {
      url = url + `&descripcion=${descripcion}`;
    }
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.lstMaestraConocimiento;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  deleteConocimiento(maeConocimientoId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraConocimiento/${maeConocimientoId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  insertConocimiento(
    body: Conocimiento
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        maestraConocimiento: {
          maeConocimientoId: body.maeConocimientoId,
          tipoConocimientoId: body.tipoConocimientoId,
          categoriaConocimientoId: body.categoriaConocimientoId,
          descripcion: body.descripcion,
          entidadId: body.entidadId,
          codigoTipoConocimiento: body.codigoTipoConocimiento,
          descripcionTipo: body.descripcionTipo,
          descripcionCategoria: body.descripcionCategoria,
          estado: body.estado,
        },
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/maestraConocimiento`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.maestraConocimiento;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  updateConocimiento(
    body: Conocimiento,
    maeConocimientoId: number
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        maestraConocimiento: {
          maeConocimientoId: body.maeConocimientoId,
          tipoConocimientoId: body.tipoConocimientoId,
          categoriaConocimientoId: body.categoriaConocimientoId,
          descripcion: body.descripcion,
          entidadId: body.entidadId,
          codigoTipoConocimiento: body.codigoTipoConocimiento,
          descripcionTipo: body.descripcionTipo,
          descripcionCategoria: body.descripcionCategoria,
          estado: body.estado,
        },
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/maestraConocimiento/${maeConocimientoId}`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.maestraConocimiento;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }


}
