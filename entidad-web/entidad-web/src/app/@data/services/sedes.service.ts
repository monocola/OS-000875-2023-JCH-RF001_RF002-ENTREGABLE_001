import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { SedesRepository } from 'src/app/@domain/repository/sede.repository';
import { requestFilter } from 'src/app/utils/general';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Entidad } from '../model/entidad';
import { ResponseRequest } from '../model/reponse-request';
import { Sede } from '../model/sede';
import { AuthenticationService } from './authentication.service';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class SedesService extends SedesRepository {
  sede: Sede[] = [];

  entidad: Entidad = null;

  constructor(
    private http: HttpClient,
    private authService: AuthenticationService
  ) {
    super();
  }

  getSedesByFiltro(body: any): Observable<Sede[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    let url = '';
    if (body) {
      if (body.sede) {
        body.sede = body.sede.sedeId;
      }
      const params = requestFilter(body);
      url = `${Const.API_ENTIDAD}v1/sede/filter?entidadId=${entidadId}&${params}`;
    } else {
      url = `${Const.API_ENTIDAD}v1/sede/filter?entidadId=${entidadId}`;
    }
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        response.payload.listarSede.map(
          (item) => (item.representante = item.nombreRepresentante)
        );
        return response.payload.listarSede;
      })
    );
  }

  registerOrUpdateSede(body, sedeId?): Observable<boolean> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        sedeId: sedeId ? sedeId : null,
        entidadId: entidadId,
        padreSedeId: null,
        estadoRegistro: body.estado,
        direccion: body.direccion,
        ubigeo: body.distrito,
        nombreSede: body.nombreSede,
        representante: body.representante,
        telefono: body.telefono,
        anexo: body.anexo,
        ambito: null,
      },
    };

    let url = '';

    if (sedeId) {
      url = `${Const.API_ENTIDAD}v1/sede/${sedeId}`;
      return this.http.put(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.payload.sede) {
            return true;
          } else {
            return false;
          }
        })
      );
    } else {
      url = `${Const.API_ENTIDAD}v1/sede`;
      return this.http.post(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.payload.sede) {
            return true;
          } else {
            return false;
          }
        })
      );
    }
  }

  deleteSede(sedeId: any): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/sede/${sedeId}?estado=0`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.payload.sede) {
          return true;
        } else {
          return false;
        }
      })
    );
  }
}
