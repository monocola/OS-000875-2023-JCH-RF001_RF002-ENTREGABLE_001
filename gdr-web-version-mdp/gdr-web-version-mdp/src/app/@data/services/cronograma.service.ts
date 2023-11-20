import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CronogramaRepository } from 'src/app/@domain/repository/cronograma.repository';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Organo } from '../model/organo';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
@Injectable({
  providedIn: 'root',
})
export class CronogramaService implements CronogramaRepository {
  organos: Organo[] = [];
  organoToEditFromChart: any = null;
  organigrama: any[] = [];

  constructor(
    private http: HttpClient,
  ) { }

  registerOrUpdateAct(body, flagReg): Observable<boolean> {
    if ( flagReg ) {
      const url = `${Const.API_PLANIFICACION}v1/actividad/crear`;
      return this.http.post(url, body).pipe(
        map((response: ResponseRequest) => {
          return !!response.payload;
        })
      );
    } else {
      const url = `${Const.API_PLANIFICACION}v1/actividad/editar`;
      return this.http.put(url, body).pipe(
        map((response: ResponseRequest) => {
          return !!response.payload;
        })
      );
    }
  }

  getCronogramas(item: any, body: any) {
    let url;
    let etapaId = body?.etapa;
    url = `${Const.API_PLANIFICACION}v1/actividad/listar?idCronograma=${item.cronogramaId}&idEtapa=${etapaId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }

  getResoluciones(item: any) {
    let url;
     url = `${Const.API_PLANIFICACION}v1/resolucion/cronograma/${item.cronogramaId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
          return response;
      })
    );
  }


  getHistorialModificacionAct(body: any, id: number) {
    let url;
    url = `${Const.API_PLANIFICACION}v1/actividad/historial/${id}?`;
    if (body.actividad !== '' && body.etapa === '' && body.resolucion === '') {
      url = url + `actividadId=${body.actividad}`;
    } else if (body.actividad === '' && body.etapa !== '' && body.resolucion === '') {
      url = url + `etapaId=${body.etapa}`;
    } else if (body.actividad === '' && body.etapa === '' && body.resolucion !== '') {
      url = url + `resolucionId=${body.resolucion}`;
    } else if (body.actividad !== '' && body.etapa !== '' && body.resolucion === '') {
      url = url + `actividadId=${body.actividad}&etapaId=${body.etapa}`;
    } else if (body.actividad === '' && body.etapa !== '' && body.resolucion !== '') {
      url = url + `etapaId=${body.etapa}&resolucionId=${body.resolucion}`;
    }  else if (body.actividad !== '' && body.etapa === '' && body.resolucion !== '') {
      url = url + `actividadId=${body.actividad}&resolucionId=${body.resolucion}`;
    } else if ( body.actividad !== '' && body.etapa !== '' && body.resolucion !== '') {
      url = url + `actividadId=${body.actividad}&etapaId=${body.etapa}&resolucionId=${body.resolucion}`;
    }
    // url = `${Const.API_PLANIFICACION}v1/actividad/historial/13`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }

  deleteCronograma(cronogramaId: any, actividadId: any): Observable<boolean> {
    const url = `${Const.API_PLANIFICACION}v1/actividad/eliminar/${cronogramaId}/${actividadId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        return response.status.success;
      })
    );
  }

  downloadExcel(cronogramaId: any, anio: string) {
      const url = `${Const.API_PLANIFICACION}v1/cronograma/downloadFormat?idCronograma=${cronogramaId}&anio=${anio}`;
      return this.http.get(url).pipe(
          map(
              (response: ResponseRequest) => {
                  if (!response.status.success) {
                      throw new Error(response.status.error.messages[0]).message;
                  } else {
                      return response.payload;
                  }
              }
          )
      );
  }

  habilitaEdicion(cronogramaId: number, flagEdicion: string): Observable<any> {
    const body = null;
    const url = `${Const.API_PLANIFICACION}v1/cronograma/${cronogramaId}/editar/${flagEdicion}`;
    return this.http.put(url, body).pipe(
      map((response: ResponseRequest) => {
        if (response) {
          return response;
        }
      })
    );
  }

  getCboActividadesResolucion(cronogramaId: number, resolucionId: number): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/actividad/historial/${cronogramaId}/${resolucionId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response;
      })
    );
  }
  getCboActividades(cronogramaId: number): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/actividad/historial/${cronogramaId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

}

export interface Persona {
  nombres: string;
  apellidoPaterno: string;
  apellidoMaterno: string;
  personaResponsableId: number;
  nombreCompleto?: string;
}
