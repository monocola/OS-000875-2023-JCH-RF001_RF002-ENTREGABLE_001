import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { requestFilter } from 'src/app/utils/general';
import { Observable } from 'rxjs';
import { MaestraRepository } from '../../@domain/repository/maestra.reposity';
import { CabeceraMaestra } from '../model/cabeceraMaestra';
import { CabeceraMaestraDetail } from '../model/cabeceraMaestraDetail';
import { ResponseRequest } from '../model/reponse-request';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';

import { Const } from './const';
import { map } from 'rxjs/operators';
import {
  ConocimientoAgrupado,
  ConocimientoDataTratada,
  MaestraConocimiento
} from '../model/maestra/conocimiento';
import { ResponseGeneric } from '../model/generic/response.generic';

@Injectable({
  providedIn: 'root',
})
export class MaestraService extends MaestraRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository

  ) {
    super();
  }

  getMaestraConocimiento(codtipo: number): Observable<ConocimientoDataTratada> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraConocimiento?&tipoConocimiento=${codtipo}`;
    return this.http.get<ResponseGeneric<MaestraConocimiento>>(url).pipe(
      map(response => {
        let result: ConocimientoDataTratada = new ConocimientoDataTratada();
        let listresult: ConocimientoAgrupado[] = [];
        response.payload.lstMaestraConocimiento.forEach(item => {
          let categoria = {
            descripcionCategoria: item.descripcionCategoria,
            hijos: [],
          };
          if (listresult.find(dataresult => item.descripcionCategoria === dataresult.descripcionCategoria) == null) {
            listresult.push(categoria);
          }
        });
        listresult.forEach(item => {
          item.hijos.push(...response.payload.lstMaestraConocimiento.filter(original => original.descripcionCategoria === item.descripcionCategoria).map(data => data.descripcion));
        });
        result.conocimientoAgrupado = listresult;
        result.listaoriginal = response.payload.lstMaestraConocimiento;
        return result;
      })
    );
  }

  getMaestraList(): Observable<CabeceraMaestra[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraCabecera/combo`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaCabecera;
      })
    );
  }

  getMaestraDetalleByCod(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraDetalles.filter(
          (m) => m.estadoRegistro !== '0'
        );
      })
    );
  }


  getComboSection(codigo_cabecera: string): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalleEntidad?idEntidad=${entidadId}&idCabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.listaCabecera[0].listMaestraDetalles;
      })
    );
  }

  /*--------combo seccion-----*/

  getMaestraDetalleByCodandCodProg(
    codigo_cabecera: string,
    codProg: number
  ): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}&codProg=${codProg}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.maestraDetalles.filter(
          (m) => m.estadoRegistro !== '0'
        );
      })
    );
  }

  getMaestraListDetail(
    idTablaMaestra?: number,
    entidadId?: number
  ): Observable<CabeceraMaestraDetail[]> {
    let url = `${Const.API_CONVOCATORIA}v1/maestraCabecera?`;
    if (idTablaMaestra) {
      url = url + `&idCabecera=${idTablaMaestra}`;
    }
    if (entidadId) {
      url = url + `&idEntidad=${entidadId}`;
    }
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!entidadId) {
          const list: CabeceraMaestraDetail[] =
            response.payload.listaCabecera[0].listMaestraDetalles;
          list.forEach((item) => {
            item.textToSearch =
              item.descripcion + ' / ' + item.descripcionCorta;
            item.descripcionMaestra =
              response.payload.listaCabecera[0].descripcion;
          });
          return list;
        } else {
          return response.payload.listaCabecera;
        }
      })
    );
  }

  filtrarMaestraDetalle(body: any): Observable<CabeceraMaestraDetail[]> {
    const tablaMaestra = body.tablaMaestra;
    const tramaToSend = body;
    tramaToSend.texto = body.nombreCamposMaestra?.descripcion || '';
    tramaToSend.nombreCamposMaestra = '';
    const params = requestFilter(tramaToSend);
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?idCabecera=${tablaMaestra}&${params}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        const list: CabeceraMaestraDetail[] = response.payload.maestraDetalles;
        list.forEach((item) => {
          item.textToSearch = item.descripcion + ' / ' + item.descripcionCorta;
          item.descripcionMaestra =
            response.payload.maestraCebecera.descripcion;
        });
        return list;
      })
    );
  }

  deleteMaestraDetalle(maeDetalleId: number): Observable<boolean> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/${maeDetalleId}?estado=0`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.payload) {
          return true;
        } else {
          return false;
        }
      })
    );
  }

  createOrUpdateMaestraDetalle(body: any, maeDetalleId?): Observable<boolean> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        maeCabeceraId: body.tablaMaestra,
        nombreCompleto: body.nombreCompleto,
        nombreCorto: body.nombreCorto,
        sigla: body.sigla,
        referencia: body.referencia,
        estado: body.estado,
      },
    };

    let url = '';

    if (maeDetalleId) {
      url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/${maeDetalleId}`;
      return this.http.put(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.payload.maestraDetalle) {
            return true;
          } else {
            return false;
          }
        })
      );
    } else {
      url = `${Const.API_CONVOCATORIA}v1/maestraDetalle`;
      return this.http.post(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.payload.maestraDetalle) {
            return true;
          } else {
            return false;
          }
        })
      );
    }
  }
  downloadBase64(url: string): Observable<boolean> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: url,
    };
    const uri = `${Const.API_MAESTRA}v1/file/downloadBase64`;
    return this.http.post(uri, tramaEnvio).pipe(
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
