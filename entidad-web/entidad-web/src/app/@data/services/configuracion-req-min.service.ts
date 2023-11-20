import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { ConfiguracionReqMinRepository } from "src/app/@domain/repository/configuracion-req-min.repository";
import { ResponseRequest } from '../model/reponse-request';
import { armarPayload } from 'src/app/utils/utils';

@Injectable({
  providedIn: 'root',
})
export class ConfiguracionReqMinService implements ConfiguracionReqMinRepository {
  headers = new HttpHeaders().set('Content-Type', 'application/json');

  constructor(
    private http: HttpClient,
  ) { }

  getCarreras(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/carreras/${perfilId}/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getEspecialidades(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/especialidad/${perfilId}/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getDeclaracionJuradaByPerfil(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/declaracionJurada/${perfilId}/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.listDeclaracionJurada;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getEspecificaciones(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/especificaciones/${perfilId}/${baseId}`;
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

  getRequisitosAdicionales(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/requisitosAdicionales/${perfilId}/${baseId}`;
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

  getExperiencialLaboral(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/experiencialLaboralByPerfil/${perfilId}/${baseId}`;
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

  getPuntajesResumen(perfilId: number, regimenId: number, entidadId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/puntajes/${perfilId}/${regimenId}/${entidadId}`;
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


  getPesosConfiguracion(perfilId: number, baseId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/pesos/${perfilId}/${baseId}`;
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

  setConfiguracionPerfil(payload: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracionPerfil`;
    return this.http.post(url, armarPayload(payload)).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  obtenerPlantillaPorPerfil(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/perfilFormato/${perfilId}/${baseId}`;
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

  getPlantillasConfiguracion(): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=DES_PLANTILLA_CONFIG`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        const lista = [];
        response.payload.maestraDetalles.forEach((element) => {
          if (element.estadoRegistro !== '0') {
            lista.push({
              maeDetalleId: element.maeDetalleId,
              descripcion: element.descripcion,
              codProg: element.codProg,
              orden: element.orden,
            });
          }
        });
        return lista;
      })
    );
  }

  getGradosCarreras(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/carrerasOtrosGrados/${perfilId}/${baseId}`;
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

  setEliminarPerfil(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/actualizaEstado/${perfilId}/${baseId}`;
    return this.http.put(url, null, { headers: this.headers }).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getListaInvestigacionYPublicacion(perfilId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/investigacion/${perfilId}/${baseId}`;
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
