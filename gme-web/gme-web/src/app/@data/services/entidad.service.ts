import { HttpClient, HttpHeaders, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { catchError, map, timeout } from 'rxjs/operators';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { EntidadRepository } from 'src/app/@domain/repository/entidad.repository';
import { ResumenServidoresCiviles } from '../model/entity';
import { ResponseRequest } from '../model/reponse-request';
import { ServidoresCivilesGraficosDonats } from '../model/servidoresCivilesGraficosDonats';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class EntidadService implements EntidadRepository {
  constructor(
    private http: HttpClient
  ) {}

  subirDocumentoEntidad(body: any, archivo: any): Observable<any> {
    const paramFD: FormData = new FormData();
    paramFD.append('archivo', archivo);
    paramFD.append('extensiones', body.extensiones);
    paramFD.append('ruc', body.ruc);
    paramFD.append('ruta', body.ruta);

    const url = `${Const.API_PLANIFICACION}v1/uploadFile/alfresco`;

    const req = new HttpRequest('POST', url, paramFD, {
      reportProgress: true,
      responseType: 'json',
    });
    console.log(req);
    return this.http.request(req);
  }

  actualizarEntidad(request: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidad: {
          abreviatura: request.abreviatura,
          nivelGobiernoId: request.nivelGobiernoId,
          sectorId: request.sectorId,
          tipoEntidadId: request.tipoEntidadId,
          base64Image: request.base64Image,
          nrosSindicatos: request.nrosSindicatos,
          sigla: request.sigla,
        },
        logo: {
          flag: 0,
          fileBase64: request.base64Image,
          fileName: request.rucEntidad + '.' + request.extension,
        },
        ruc: request.rucEntidad,
        razonSocial: request.razonSocial,
        actualizaRazon: request.actualizaRazon,
      },
    };
    const url = `${Const.API_ENTIDAD}v1/actualizar/entidad/${request.entidadId}`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        console.info(response);
        return response;
      })
    );
  }

  getListaResumensServidoresCiviles(): Observable<ResumenServidoresCiviles[]> {
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;

    const url =
      Const.API_ENTIDAD + `v1/entidad/resumen-servidores-civiles/${entidadId}`;
    const headers = new HttpHeaders().set('Content-Type', 'application/json');
    return this.http.get<ResumenServidoresCiviles[]>(url, { headers }).pipe(
      map((response: any) => {
        return response.payload.servidoresCiviles; // CE de ApiPersona o DNI de Reniec
      }),
      timeout(5000),
      catchError((e) => {
        throw new Error(
          'Hubo un error al traer resumen servidores civiles: ' + e.message
        ).message;
      })
    );
  }

  getListaResumensServidoresCivilesGraficosDonats(): Observable<ServidoresCivilesGraficosDonats> {
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;

    const url =
      Const.API_ENTIDAD +
      `v1/entidad/gme/resumen-servidores-civiles/graficos-donats/${entidadId}`;
    const headers = new HttpHeaders().set('Content-Type', 'application/json');
    return this.http
      .get<ServidoresCivilesGraficosDonats>(url, { headers })
      .pipe(
        map((response: any) => {
          console.log(response.payload);
          return response.payload; // CE de ApiPersona o DNI de Reniec
        }),
        timeout(5000),
        catchError((e) => {
          throw new Error(
            'Hubo un error al traer datos servidores civiles graficos: ' +
              e.message
          ).message;
        })
      );
  }

  searchSigla(sigla: any): Observable<any[]> {
    const url = `${Const.API_ENTIDAD}v1/entidad/by/?sigla=${sigla}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.entidad;
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
