import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { MaestraEntidadRepository } from '../../@domain/repository/maestra-entidad.repository';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';
import { concatMap, map } from 'rxjs/operators';
import { MaestraService } from './maestra.service';

@Injectable({
  providedIn: 'root',
})
export class MaestraEntidadService extends MaestraEntidadRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository,
    private maestraService: MaestraService
  ) {
    super();
  }

  asignaMaestraDetalle(cabeceraId: number, detalleId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad/asignaMaestraDetalle`;

    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidadId: this.authenticationRepository.getCurrentUserValue.entidadId,
        maeCabeceraId: cabeceraId,
        maeDetalleId: detalleId,
      },
    };

    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return true;
        }
      })
    );
  }

  asignaMaestraDetalleEntidad(cabeceraId: number, detalleId: number, entidadId: number=null): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad/asignaMaestraDetalle`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidadId: entidadId,
        maeCabeceraId: cabeceraId,
        maeDetalleId: detalleId,
      },
    };

    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return true;
        }
      })
    );
  }

  actualizaConfigMaestraDetalle(
    configMaestraId: number,
    estado: boolean
  ): Observable<any> {
    const url = `${
      Const.API_CONVOCATORIA
    }v1/maestraDetalleEntidad/configMaestraDetalle/${configMaestraId}?estado=${
      estado ? '1' : '0'
    }`;
    return this.http.put(url, null).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return true;
        }
      })
    );
  }

  getMaestraDetalleEntidad(idCabecera?: number): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad?idEntidad=${entidadId}`;
    if (idCabecera) {
      url = url + `&idCabecera=${idCabecera}`;
    }
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaCabecera;
        }
      })
    );
  }

  getCamposAsignadosByCodCabecera(COD_CABECERA?: string): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    return this.maestraService.getMaestraList().pipe(
      concatMap((res) => {
        const idCabecera = res.filter(
          (el) => el.codigoCabecera === COD_CABECERA
        )[0].maeCabeceraId;

        let url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad?idEntidad=${entidadId}&idCabecera=${idCabecera}`;
        return this.http.get(url).pipe(
          map((response: ResponseRequest) => {
            if (!response.status.success) {
              throw new Error(response.status.error.messages[0]).message;
            } else {
              return (
                response.payload.listaCabecera[0].listMaestraDetalles.filter(
                  (l) => l.estadoRegistro === '1'
                ) || []
              );
            }
          })
        );
      })
    );
  }

  getMaeDetalleEntByCod(codProg?: string): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    return this.maestraService.getMaestraList().pipe(
      concatMap((res) => {
        const idCabecera = res.filter((el) => el.codigoCabecera === codProg)[0]
          .maeCabeceraId;
        let url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad?idEntidad=${entidadId}&idCabecera=${idCabecera}`;
        return this.http.get(url).pipe(
          map((response: ResponseRequest) => {
            if (!response.status.success) {
              throw new Error(response.status.error.messages[0]).message;
            } else {
              return (
                response.payload.listaCabecera[0].listaMaestraDetalleEntidads ||
                []
              );
            }
          })
        );
      })
    );
  }

  createOrUpdateMaestraDetalle(
    body: any,
    maeCabeceraId: number,
    maeDetalleEntidadId?: boolean
  ): Observable<boolean> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidadId,
        maeCabeceraEntidadId: maeCabeceraId,
        nombreCompletoEntidad: body.nombre,
        nombreCortoEntidad: body.nombreCorto,
        siglaEntidad: body.sigla,
        referenciaEntidad: body.referencia,
        estado: body.estado,
      },
    };

    if (maeDetalleEntidadId) {
      const url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad/${maeDetalleEntidadId}`;
      return this.http.put(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.status.success) {
            return true;
          } else {
            return false;
          }
        })
      );
    } else {
      const url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad`;
      return this.http.post(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.status.success) {
            return true;
          } else {
            return false;
          }
        })
      );
    }
  }

  getRegimenModalidad(regimen: number, modalidad: number) {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;

    let url = `${Const.API_CONVOCATORIA}v1/base/maestra?entidad=${entidadId}`;
    if (regimen !== null) {
      url += `&regimen=${regimen}`;
    }

    if (modalidad !== null) {
      url += `&modalidad=${modalidad}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.lstMaestraEntidad;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
}
