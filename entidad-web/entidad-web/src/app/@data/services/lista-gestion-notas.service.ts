import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ListaGestionNotasRepository } from 'src/app/@domain/repository/lista-gestion-notas.repository';
import { armarPayload } from 'src/app/utils/utils';
import { NotaIndividual, NotaMasiva } from '../model/lista-evaluaciones/entity';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class ListaGestionNotasService extends ListaGestionNotasRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  getListaEvaluacionesPostulante(
    filtro: any,
    convocatoriaId: string
  ): Observable<any> {
    let requests = armarPayload<any>(filtro);
    const url = `${Const.API_EVALUACION}v1/evaluaciones/${convocatoriaId}`;
    return this.http.post(url, requests).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
  }

  getDataTableAll(convocatoriaId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/listarBandejaEvaluacion/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
  }

  getComboEvaluaciones(convocatoriaId: string): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/comboEvaluacion/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getComboGrupos(convocatoriaId: string, perfilId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/comboGrupo/${convocatoriaId}/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getComboExamen(convocatoriaId: string, perfilId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/comboExamen/${convocatoriaId}/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getComboPostulantes(programacionId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/postulantes/${programacionId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getPDFcv(cabeceraId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/notaPdf/postulante/${cabeceraId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  guardarNotaExamenIndividual(individual: NotaIndividual): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/nota`;
    let payload = { nota: individual };
    let request = armarPayload<any>(payload);
    return this.http.post(url, request).pipe(
      map((response: ResponseRequest) => {
        return response;
        /* if (response.status.success) {
          return response;
        } else {
          return response.status.error.messages[0];
        } */
      })
    );
  }

  guardarNotaExamenMasivo(request: NotaMasiva): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/nota/masiva`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        programacionId: request.programacionId,
        documentoBase64: request.documentoBase64,
      },
    };

    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response;
      })
    );
  }

  downloadTemplate(programacionId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/nota/masiva/${programacionId}`;

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getComboEvaluacionesPostulante(convocatoriaId: number): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/base/evaluacion/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        const lista = [];
        response.payload.baseEvaluacionDetalleDTOList.forEach((element) => {
          if (element.estado !== '0') {
            lista.push({
              evaluacionDetalleId: element.evaluacionDetalleId,
              descripcion: element.descripcion,
              codProg: element.codProg,
            });
          }
        });
        return lista;
      })
    );
  }

  saveNotaExamenMasivo(programacionId: number, multipartData: FormData): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/nota/masiva/${programacionId}`;
    return this.http.post(url, multipartData).pipe(
      map((response: ResponseRequest) => {
        return response;
      })
    );
  }
}
