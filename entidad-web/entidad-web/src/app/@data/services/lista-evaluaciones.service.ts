import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { ListaEvaluacionesRepository } from 'src/app/@domain/repository/lista-evaluaciones.repository';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Injectable({
  providedIn: 'root',
})
export class ListaEvaluacionesService extends ListaEvaluacionesRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  getListaEvaluaciones(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/listaConocimiento/`;
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;

    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        convocatoriaId: null,
        idperfil: null,
        fecha: null,
        estadoExamen: null,
        entidadId: entidadId,
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaConocimiento;
      })
    );
  }
}
