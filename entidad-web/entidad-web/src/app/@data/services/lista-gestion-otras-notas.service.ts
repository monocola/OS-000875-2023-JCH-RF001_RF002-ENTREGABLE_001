import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ListaGestionOtrasNotasRepository } from 'src/app/@domain/repository/lista-gestion-otras-notas.repository';
import { armarPayload } from 'src/app/utils/utils';
import { OtraNotaIndividual } from '../model/lista-evaluaciones/entity';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class ListaGestionOtrasNotasService extends ListaGestionOtrasNotasRepository {
  constructor(private http: HttpClient) {
    super();
  }

  getComboPerfil(convocatoriaId: string): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/convocatoria/${convocatoriaId}`;
    // const url = `${Const.API_CONVOCATORIA}v1/perfil/convocatoria/10`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getComboPostulantes(
    convocatoriaId: string,
    perfilId: number
  ): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulantes/convocatoriaperfil/${convocatoriaId}/${perfilId}`;
    // const url = `${Const.API_SELECCION}v1/postulantes/convocatoriaperfil/56/10`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  guardarOtraNotaIndividual(
    individual: OtraNotaIndividual,
    convocatoriaId: number
  ): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/registrarOtraEvaluacion/${convocatoriaId}/${individual.perfilId}`;
    let payload = { otrasEvaluaciones: individual };
    console.log(payload);
    let request = armarPayload<any>(payload);
    return this.http.post(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return 'Registro guardado exitosamente';
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  guardarCondicionMasivo(objSave: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/otrasevaluaciones/masivo/${objSave.convocatoriaId}/${objSave.perfilId}/${objSave.tipoEvaluacion}`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        observacion: objSave.observacion,
        fechaEvaluacion: objSave.fechaEvaluacion,
        documentoBase64: objSave.documentoBase64,
      },
    };
    console.log(tramaEnvio);
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response;
      })
    );
  }

  downloadTemplate(): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/otrasEvaluaciones/masiva`;

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

  listaEstadoEvaluacion(flag: any) {
    const url = `${Const.API_SELECCION}v1/evaluacion/listaEstadoEvaluacionCurricular/${flag}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  guardarCondicionMasivoMtp(objSave: any, multipartData: FormData): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/otrasevaluaciones/masivo/${objSave.convocatoriaId}/${objSave.perfilId}/${objSave.tipoEvaluacion}`;
    return this.http.put(url, multipartData).pipe(
      map((response: ResponseRequest) => {
        return response;
      })
    );
  }

  guardarOtraNotaIndividualMtp(multipartData: FormData, perfilId: number, convocatoriaId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/registrarOtraEvaluacion/${convocatoriaId}/${perfilId}`;
    return this.http.post(url, multipartData).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return 'Registro guardado exitosamente';
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

}
