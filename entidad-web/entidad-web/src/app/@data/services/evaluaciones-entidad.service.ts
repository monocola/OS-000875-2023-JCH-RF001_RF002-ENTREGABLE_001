import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { requestFilter } from 'src/app/utils/general';
import { Observable } from 'rxjs';
import { catchError, map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { EvaluacionesEntidadRepository } from 'src/app/@domain/repository/evaluaciones-entidad.repository';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Injectable({
  providedIn: 'root',
})
export class EvaluacionesEntidadService extends EvaluacionesEntidadRepository {
  entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;

  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  asignarEvaluacionesEntidad(listaJerarquia: any[]): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/evaluacion/entidad/asignarEvaluacion`;
    const lstJerarquia = listaJerarquia.map((jerarquia) => {
      return {
        codigoNivel1: jerarquia.regimenId,
        codigoNivel2: jerarquia.modalidadId,
        codigoNivel3: jerarquia.tipoId,
      };
    });
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidadId: this.entidadId,
        lstJerarquia,
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return true;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        },
        catchError((e) => {
          throw e;
        })
      )
    );
  }

  updateEvaluacionEntidad(
    evaluaciones: any[],
    jerarquiaId: any
  ): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/evaluacionEntidad/${jerarquiaId}`;
    const listaEvaluacion = evaluaciones.map((e, index) => {
      return {
        evaluacionId: null,
        jerarquiaId: jerarquiaId,
        tipoEvaluacionId: e.tipoEvaluacionId,
        orden: index + 1,
        peso: e.peso,
        puntajeMinimo: e.puntajeMinimo,
        puntajeMaximo: e.puntajeMaximo,
        detalleEvaluacion: e.detalleEvaluacion,
        estado: e.estado,
        entidadId: this.entidadId,
        evaluacionEntidad: e.evaluacionEntidad,
      };
    });
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        listaEvaluacion,
      },
    };
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response.status.success;
      })
    );
  }

  eliminarJerarquia(jerarquiaId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/evaluacion/entidad/eliminarJerarquia`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        jerarquiaId: jerarquiaId,
        entidadId: this.entidadId,
      },
    };
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response.status.success;
      })
    );
  }

  searchEnlacesRegimen(body: any): Observable<any> {
    const tramaEnvio = {
      idCabecera: body.regimen,
      modalidadId: body.modalidad,
      tipoId: body.tipo,
      estadoId: body.estado,
    };
    const filterString = requestFilter(tramaEnvio);
    const url = `${Const.API_CONVOCATORIA}v1/evaluaciones?${filterString}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }
}
