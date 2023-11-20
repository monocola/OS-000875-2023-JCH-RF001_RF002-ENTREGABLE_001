import {
  ListaConocimiento,
  Regimen,
  ResumenRespuestas,
  RespDatoConvocatoria,
} from './../model/lista-evaluaciones/entity';
import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { Convocatoria } from '../model/lista-evaluaciones/entity';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { armarPayload } from 'src/app/utils/utils';

@Injectable({
  providedIn: 'root',
})
export class SeleccionServirService extends SeleccionServirRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  getConvocatoriasEntidad(entidadId: number): Observable<Convocatoria[]> {
    const url = `${Const.API_SELECCION}v1/postulacion/convocatoria/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getRegimenEntidad(entidadId: number): Observable<Regimen[]> {
    const url = `${Const.API_SELECCION}v1/postulacion/regimen/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getListaConocimientos(request: any): Observable<ListaConocimiento[]> {
    const url = `${Const.API_EVALUACION}v1/evaluacion/listaConocimiento/`;
    return this.http.post(url, request).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaConocimiento;
      })
    );
  }

  listarExamenCorregido(cabeceraId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/examen/corregido/${cabeceraId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }
  getCapturaImagenPostulante(
    examenId: number,
    postulanteId: number
  ): Observable<any> {
    const url = `${Const.API_EVALUACION}/v1/examen/capturas/${examenId}/${postulanteId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  getPreguntasCorregidas(cabeceraId: number): Observable<ResumenRespuestas> {
    const url = `${Const.API_EVALUACION}v1/pregunta/corregido/${cabeceraId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  getDetalleExamen(cabeceraId: number): Observable<ResumenRespuestas> {
    const url = `${Const.API_EVALUACION}v1/detalle/examen/corregido/${cabeceraId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  getDatosConvocatoriaById(
    convocatoriaId: number
  ): Observable<RespDatoConvocatoria> {
    const url = `${Const.API_SELECCION}v1/postulacion/getconvocatoria/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  getTipoContrato(regimen: number): Observable<RespDatoConvocatoria> {
    const url = `${Const.API_CONVOCATORIA}v1/contrato/tipo?regimen=${regimen}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  getPostulantesByConvocatoria(
    baseId: number,
    perfilId: number,
    estadoId: number,
    rncss: number,
    reqmin: number,
    fechaIni: string,
    fechaFin: string,
    page: number,
    size: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_SELECCION}v1/convocatoria/${entidadId}/${baseId}?page=${page}&size=${size}`;

    if (perfilId !== 0) {
      url = url + `&perfil=${perfilId}`;
    }
    if (estadoId !== null) {
      url = url + `&califica=${estadoId}`;
    }
    if (rncss !== null) {
      url = url + `&redam=${rncss}`;
    }
    if (reqmin !== 0) {
      url = url + `&reqmin=${reqmin}`;
    }
    if (fechaIni !== '') {
      url = url + `&fechaIni=${fechaIni}`;
    }

    if (fechaFin !== '') {
      url = url + `&fechaFin=${fechaFin}`;
    }

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
  GetPostulanteEtapaReclutamiento(
    baseId: number,
    perfilId: number,
    estadoId: number,
    rncss: number,
    reqmin: number,
    fechaIni: string,
    fechaFin: string,
    page: number,
    size: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_SELECCION}v1/convocatoriaEtapaReclutamiento/${entidadId}/${baseId}?page=${page}&size=${size}`;

    if (perfilId !== 0) {
      url = url + `&perfil=${perfilId}`;
    }
    if (estadoId !== 0) {
      url = url + `&estado=${estadoId}`;
    }
    if (rncss !== 0) {
      url = url + `&rncss=${rncss}`;
    }
    if (reqmin !== 0) {
      url = url + `&reqmin=${reqmin}`;
    }
    if (fechaIni !== '') {
      url = url + `&fechaIni=${fechaIni}`;
    }

    if (fechaFin !== '') {
      url = url + `&fechaFin=${fechaFin}`;
    }

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

  EtapaProgresoReclutamiento(baseId: number, etapaId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}/v1/basecronogramas/etapa/${etapaId}/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  updateFlagRedam(request: any) {
    const url = `${Const.API_SELECCION}/v1/convocatoria/redam`;
    return this.http.put(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  FiltroEleccion(
    baseId: number,
    valor: string,
    page: number,
    size: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_SELECCION}v1/convocatoria/like/${entidadId}/${baseId}/${valor}?page=${page}&size=${size}`;

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

  guardarPregAbiertas(cabeceraId: number, req: any) {
    const url = `${Const.API_EVALUACION}v1/califica/abierta/${cabeceraId}`;
    let request = armarPayload<any>(req);
    return this.http.put(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        }
      })
    );
  }

  getInterpolacion(entidadId: number, baseId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/convocatoria/interpolacion/${entidadId}/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response;
      })
    );
  }
}
