import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { SeguimientoRepository } from '../../@domain/repository/seguimiento.repository';
import { ComboitemModel } from '../model/generic/comboitem.model';
import { ResponseGeneric } from '../model/generic/response.generic';
import { ResponseMaestra } from '../model/maestra/maestra';
import { SegimientoResponse } from '../model/segimiento/seguimientoresponse';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';

@Injectable({
  providedIn: 'root',
})
export class SeguimientoService extends SeguimientoRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  getFiltroMaestra(keymae: string): Observable<ComboitemModel[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${keymae}`;
    return this.http.get<ResponseGeneric<ResponseMaestra>>(url).pipe(
      map((response) => {
        return response.payload.maestraDetalles.filter(
          (m) => m.estadoRegistro !== '0'
        );
      }),
      map((listdata) => {
        return listdata.map((item) => {
          return {
            value: item.maeDetalleId,
            description: item.descripcion,
            codProg: +item.codProg,
          };
        });
      })
    );
  }

  getData(filtros: any, page: number, size: number): Observable<any> {
    // const url = `${Const.API_CONVOCATORIA}v1/convocatorias/${this.authenticationRepository.getCurrentUserValue.entidadId}?size=10`;
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_CONVOCATORIA}v1/convocatorias/${entidadId}?page=${page}&size=${size}`;
    return this.http
      .get<ResponseGeneric<SegimientoResponse>>(url, { params: filtros })
      .pipe(
        map((response) => {
          return response.payload;
        })
      );
  }

  getDataSeguimiento(
    filtros: any,
    page: number,
    size: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_CONVOCATORIA}v1/convocatorias/${entidadId}?page=${page}&size=${size}`;

    return this.http
      .get<ResponseGeneric<SegimientoResponse>>(url, { params: filtros })
      .pipe(
        map((response) => {
          return response.payload;
        })
      );
  }

  getComunicados(
    convocatoriaId: number,
    perfilId: number,
    etapaId: number,
    fechaIni: string,
    fechaFin: string,
    tipoComunicadoId: number,
    estadoId: number,
    page: number,
    size: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    // let url = `${Const.API_CONVOCATORIA}v1/comunicados/${entidadId}?`;
    let url = `${Const.API_CONVOCATORIA}v1/comunicados/${entidadId}?page=${page}&size=${size}`;

    if (convocatoriaId !== 0) {
      url = url + `&convocatoria=${convocatoriaId}`;
    }

    if (perfilId !== 0) {
      url = url + `&perfil=${perfilId}`;
    }

    if (etapaId !== 0) {
      url = url + `&etapa=${etapaId}`;
    }

    if (fechaIni !== '') {
      url = url + `&fechaIni=${fechaIni}`;
    }

    if (fechaFin !== '') {
      url = url + `&fechaFin=${fechaFin}`;
    }

    if (tipoComunicadoId !== 0) {
      url = url + `&tipoComunicado=${tipoComunicadoId}`;
    }

    if (estadoId !== 0) {
      url = url + `&estado=${estadoId}`;
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

  saveComunicado(
    convocatoriaId: any,
    perfilId: any,
    etapaId: any,
    tipoComunicadoId: any,
    file: any,
    estadoId: any
  ): Observable<any> {
    const nombreGestor = this.authenticationRepository.getCurrentUserValue
      .nombreCompleto;
    const idGestor: any = this.authenticationRepository.getCurrentUserValue
      .personaId;

    let formData = new FormData();
    formData.append('convocatoriaId', convocatoriaId);
    formData.append('perfilId', perfilId);
    formData.append('etapaId', etapaId);
    formData.append('estadoId', estadoId);
    formData.append('tipoComunicadoId', tipoComunicadoId);
    formData.append('gestorId', idGestor);
    formData.append('coordinadorId', '');
    formData.append('nombreGestor', nombreGestor);
    formData.append('nombreCoordinador', '');
    formData.append('estadoRegistro', '1');
    formData.append('comunicadoId', '');
    formData.append('file', file, file.name);

    const url = `${Const.API_CONVOCATORIA}v1/comunicados`;
    return this.http.post(url, formData).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  inactivarComunicado(comunicadoId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/comunicado/${comunicadoId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.status.error.messages[0];
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getComunicado(comunicadoId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/comunicado/${comunicadoId}`;

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

  updateComunicado(
    comunicadoId: any,
    convocatoriaId: any,
    perfilId: any,
    etapaId: any,
    estadoId: any,
    tipoComunicadoId: any,
    gestorId: any,
    coordinadorId: any,
    nombreGestor: any,
    nombreCoordinador: string,
    file: any
  ): Observable<any> {
    let formData = new FormData();
    formData.append('convocatoriaId', convocatoriaId);
    formData.append('perfilId', perfilId);
    formData.append('etapaId', etapaId);
    formData.append('estadoId', estadoId);
    formData.append('tipoComunicadoId', tipoComunicadoId);
    formData.append('gestorId', gestorId ? gestorId : '');
    formData.append('coordinadorId', coordinadorId ? coordinadorId : '');
    formData.append('nombreGestor', nombreGestor ? nombreGestor : '');
    formData.append('nombreCoordinador', nombreCoordinador ? nombreCoordinador : '');
    formData.append('estadoRegistro', '1');
    formData.append('comunicadoId', comunicadoId);
    formData.append('file', file, file.name);
    
    const url = `${Const.API_CONVOCATORIA}v1/comunicado`;
    return this.http.put(url, formData).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  updateEstadoComunicado(
    estadoId: number,
    observacion: string,
    gestorId: number,
    coordinadorId: number,
    nombreGestor: string,
    nombreCoordinador: string,
    comunicadoId: number,
    estadoConvocatoriaId: number
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        estadoId: estadoId,
        observacion: observacion,
        gestorId: gestorId,
        coordinadorId: coordinadorId,
        nombreGestor: nombreGestor,
        nombreCoordinador: nombreCoordinador,
        comunicadoId: comunicadoId,
        estadoConvocatoriaId: estadoConvocatoriaId,
      },
    };

    const url = `${Const.API_CONVOCATORIA}v1/comunicado/estado/${comunicadoId}`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  CrearContratoOrConvenio(
    tipoTrabajo: number,
    params: any,
    tipoContrato: number
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        tipoTrabajo: tipoTrabajo,
        postulantes: params,
        tipoContrato: tipoContrato,
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/contrato`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getContratosOrConvenios(
    tipo: number,
    regimen: number,
    tipoPractica: number,
    perfil: number,
    fechaInicio: string,
    fechaFin: string,
    estado: number,
    page: number,
    size: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_CONVOCATORIA}v1/contratoconvenios/filtros/${entidadId}/${tipo}?page=${page}&size=${size}`;
    if (regimen !== 0) {
      url = url + `&regimen=${regimen}`;
    }
    if (tipoPractica !== 0) {
      url = url + `&tipoPractica=${tipoPractica}`;
    }
    if (perfil !== 0) {
      url = url + `&perfil=${perfil}`;
    }
    if (fechaInicio !== '') {
      url = url + `&fechaIni=${fechaInicio}`;
    }
    if (fechaFin !== '') {
      url = url + `&fechaFin=${fechaFin}`;
    }
    if (estado !== 0) {
      url = url + `&estado=${estado}`;
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
}
