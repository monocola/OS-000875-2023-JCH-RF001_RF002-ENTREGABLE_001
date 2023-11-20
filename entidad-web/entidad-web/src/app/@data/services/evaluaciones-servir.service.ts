import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { EvaluacionesServirRepository } from 'src/app/@domain/repository/evaluaciones-servir.repository';
import { requestFilter } from 'src/app/utils/general';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class EvaluacionesServirService extends EvaluacionesServirRepository {
  constructor(private http: HttpClient) {
    super();
  }

  getEvaluaciones(): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/evaluaciones?estadoId=1`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  /*getRegimenesServir(): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/combo/regimen`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaDetalles;
      })
    );
  }*/

  getRegimenesServir(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraDetalles;
      })
    );
  }
  getRegimenesCabecera(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraCebecera;
      })
    );
  }

  /*getModalidadesServir(): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/combo/modalidad`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaDetalles;
      })
    );
  }*/

  getModalidadesServir(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraDetalles;
      })
    );
  }

  /*getTiposServir(): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/combo/tipo`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaDetalles;
      })
    );
  }*/

  getTiposServir(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraDetalles;
      })
    );
  }
  /*getEvaluacionesServir(): Observable<any[]> {
  const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/combo/evaluaciones`;
  return this.http.get(url).pipe(
    map((response: ResponseRequest) => {
      return response.payload.listaDetalles;
    })
  );*/

  getEvaluacionesServir(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraDetalles;
      })
    );
  }

  saveEnlaceRegimen(listaJerarquia): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/evaluacion`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        listaJerarquia,
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.payload.listaJerarquia) {
          return true;
        }
      })
    );
  }

  editEnlaceRegimen(listaEvaluacion, jerarquiaId): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/evaluacion/${jerarquiaId}`;
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
        if (response.payload.listaJerarquia) {
          return true;
        }
      })
    );
  }

  deleteJerarquia(
    nivel1: number,
    nivel2?: number,
    nivel3?: number
  ): Observable<any> {
    let url = `${Const.API_CONVOCATORIA}v1/evaluacion/inactivarJerarquia?codigoNivel1=${nivel1}`;
    if (nivel2) {
      url = url + `&codigoNivel2=${nivel2}`;
    }
    if (nivel3) {
      url = url + `&codigoNivel3=${nivel3}`;
    }
    url = url + `&estado=0`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        }
      })
    );
  }

  searchEnlacesRegimen(body: any): Observable<any> {
    const tramaEnvio = {
      idCabecera: body.regimen,
      modalidadId: body.modalidad,
      tipoId: body.tipo,
      estadoId: body.estado,
      entidadId: body.entidadId,
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
