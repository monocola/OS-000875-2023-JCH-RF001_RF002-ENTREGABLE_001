import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { requestFilterAll } from 'src/app/utils/general';

@Injectable({
  providedIn: 'root',
})
export class UnidadOrganicaService implements UnidadOrganicaRepository {
  unidadesOrganicas: any[] = [];

  constructor(
    private http: HttpClient,
    private authService: AuthenticationRepository
  ) {}

  createOrUpdateUnidad(
    body: any,
    flag?: boolean,
    idOrgano?: number,
    organo?: any
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        estadoRegistro: body.estado,
        entidadId: this.authService.getCurrentUserValue.entidadId,
        areaId: null,
        sedeId: null,
        padreOrganigramaId:
          body.organoQueDepende || body.undadOrganicaQueDepende,
        puesto: body.puesto,
        orden: 0,
        nivel: body.nivel,
        tipoOrganoUoId: 82, // fijo
        naturalezaOrgano: body.naturaleza,
        nivelGobiernoId: null,
        descripcion: body.nombreUnidadOrgano,
        descripcionCorta: null,
        sigla: body.sigla.toUpperCase(),
        tipoDocumento: body.tipoDocumento,
        numeroDocumento: body.numeroDocumento,
        nombres: body.nombres.toUpperCase(),
        apellidoPaterno: body.apellidoPaterno.toUpperCase(),
        apellidoMaterno: body.apellidoMaterno?.toUpperCase(),
        telefono: body.celular,
        telefonoId: organo?.telefonoId || null,
        correo: body.correoLaboral.toLowerCase(),
        correoId: organo?.correoId || null,
        paisId: body.paisObject?.paisId || null,
      },
    };

    if (flag) {
      const url = `${Const.API_ENTIDAD}v1/unidadorganica/${idOrgano}`;
      return this.http.put(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return true;
          }
        })
      );
    } else {
      const url = `${Const.API_ENTIDAD}v1/unidadorganica`;
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
  }

  getUnidadesOrganicas(toUpdateValues: boolean): Observable<any[]> {
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    const url = `${Const.API_ENTIDAD}v1/unidadorganica/${entidadId}`;
    const request = this.http.get(url).pipe(
      map((response: any) => {
        this.unidadesOrganicas = response.payload.listaUnidadOrganica;
        return response.payload.listaUnidadOrganica;
      })
    );
    return toUpdateValues
      ? request
      : this.unidadesOrganicas.length > 0
      ? of(this.unidadesOrganicas)
      : request;
  }

  deleteUnidad(organigramaId: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/organigrama/${organigramaId}?estado=0`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return true;
        }
      })
    );
  }

  downloadExcel() {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/unidadorganica/downloadFormat?idEntidad=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  uploadFileMasivo(body: any): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/unidadorganica/masivo`;
    const tramaEnvio = {
      value: body.split('base64,')[1],
      entidadId,
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((res: ResponseRequest) => {
        if (res.status.success) {
          if (res.payload.unidadOrganica.length === 0) {
            return true;
          } else {
            return res.payload;
          }
        } else {
          throw new Error(res.status.error.messages[0]).message;
        }
      })
    );
  }

  getUnidadOrganicaSup(tipoOrganoId?: number): Observable<any> {
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    const url =
      `${Const.API_ENTIDAD}v1/organigrama/unidadorganica/superior?entidadId=${entidadId}` +
      (tipoOrganoId ? `&tipoOrganoId=${tipoOrganoId}` : '');
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaComboUnidadOrganica;
        }
      })
    );
  }

  getUnidadOrganicaCbo(
    tipoOrganoId?: number,
    unidadOrganicaSuperiorId?: number
  ): Observable<any> {
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    const url =
      `${Const.API_ENTIDAD}v1/organigrama/unidadorganica/combo?entidadId=${entidadId}` +
      (tipoOrganoId ? `&tipoOrganoId=${tipoOrganoId}` : '') +
      (unidadOrganicaSuperiorId ? `&uoSupId=${unidadOrganicaSuperiorId}` : '');
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaComboUnidadOrganica;
        }
      })
    );
  }

  getPuestos(body: FiltroPuesto): Observable<any[]> {
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    const filterString = requestFilterAll(body);
    const url = `${Const.API_ENTIDAD}v1/puesto/listar?entidadId=${entidadId}&${filterString}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaPuesto;
      })
    );
  }
}
interface FiltroPuesto {
  esJefe: string;
  unidadOrganicaID: number;
  nombrePuesto: string;
  puestoId: number;
}
