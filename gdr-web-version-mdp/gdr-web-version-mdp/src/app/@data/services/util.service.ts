import { Injectable } from '@angular/core';
import { UtilRepository } from '../../@domain/repository/util.repository';
import { Observable } from 'rxjs/Observable';
import { Const } from './const';
import { HttpClient, HttpRequest } from '@angular/common/http';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';

@Injectable({
  providedIn: 'root'
})
export class UtilService implements UtilRepository {

  constructor(
    private http: HttpClient
  ) { }

  obtenerPdf(uiid: string): any {
    const url = `${Const.API_PLANIFICACION}v1/resolucion/obtenerResolucionPdf?uiid=${uiid}`;
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

  registrarResolucion(body: any, archivo: any): Observable<any> {
    const paramFD: FormData = new FormData();
    paramFD.append('archivo', archivo);
    paramFD.append('entidadId', body.entidadId);
    paramFD.append('cronogramaId', body.cronogramaId);
    paramFD.append('fechaAprobacion', body.fechaAprobacion);
    paramFD.append('descripcionResponsable', body.descripcionResponsable);
    paramFD.append('descripcionResolucion', body.descripcionResolucion);
    paramFD.append('cicloId', body.cicloId);


    const url = `${Const.API_PLANIFICACION}v1/resolucion`;
    const req = new HttpRequest(
      'POST',
      url,
      paramFD,
      {
        reportProgress: true,
        responseType: 'json',
      }
    );
    console.log(req);
    return this.http.request(req);
  }

  subirFormatoMetas(body: any, archivo: any): Observable<any> {
    const paramFD: FormData = new FormData();
    paramFD.append('archivo', archivo);
    paramFD.append('cicloId', body.cicloId);
    paramFD.append('entidadId', body.entidadId);
    paramFD.append('personaId', body.personaId);
    paramFD.append('detaUoId', body.detaUoId);

    const url = `${Const.API_PLANIFICACION}v1/servidorcivil/subirFormatoMetas`;
    const req = new HttpRequest(
      'POST',
      url,
      paramFD,
      {
        reportProgress: true,
        responseType: 'json',
      }
    );
    let algo = this.http.request(req);
    console.info(algo);
    return algo;
  }

  obtenerDocumentoPdf(uiid: string): any {
    // const url = `${Const.API_PLANIFICACION}v1/servidorcivil/descargarReporte?uiid=${uiid}`;
    const url = `${Const.API_PLANIFICACION}v1/servidorcivil/descargarReporte?uiid=${uiid}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          console.info(response);
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  obtenerDocumentoPdfSinUiid(detUOId: string, personaId: string, cicloId: string): any {

    // const url = `${Const.API_PLANIFICACION}v1/descarga/pdf?detUOId=${detUOId}&personaId=${personaId}&cicloId=${cicloId}`;
    const url = `${Const.API_PLANIFICACION}v1/descarga/pdf?detUOId=${detUOId}&personaId=${personaId}&cicloId=${cicloId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          console.info(response);
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
       // return response.payload;
        }
      )
    );
  }

}
