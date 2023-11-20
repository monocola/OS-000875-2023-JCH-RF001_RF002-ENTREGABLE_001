import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ReunionesRepository } from 'src/app/@domain/repository/reuniones.repository';
import { ResponseRequest } from '../model/reponse-request';
import { Reuniones } from '../model/reuniones';
import { Const } from './const';
@Injectable({
  providedIn: 'root',
})
export class ReunionesService implements ReunionesRepository {
  constructor(
    private http: HttpClient,
    private authService: AuthenticationRepository
  ) {}

  getPuestosTabs(): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const personaId = this.authService.getCurrentUserValue.personaId;

    const url = `${Const.API_PLANIFICACION}v1/reunion/listaUOxEvaluador?personaId=${personaId}&entidadId=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaPuesto;
      })
    );
  }

  getPuestosPorEvaluadoTabs(): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const personaId = this.authService.getCurrentUserValue.personaId;

    const url = `${Const.API_PLANIFICACION}v1/reunion/listaUOxEvaluadorPorEvaluado?personaId=${personaId}&entidadId=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaPuesto;
      })
    );
  }

  listReuniones(body: Reunion): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const personaId = this.authService.getCurrentUserValue.personaId;
    const url = `${Const.API_PLANIFICACION}v1/reunion/ciclo/${body.cicloId}?entidadId=${entidadId}&uoId=${body.uoId}&personaEvaluadorId=${personaId}&detalleUoEvaluadorId=${body.evaluadoDetalleUoId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }

  listReunionesEvaluado(body: Reunion): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const personaId = this.authService.getCurrentUserValue.personaId;
    const url = `${Const.API_PLANIFICACION}v1/reunion/ciclo/${body.cicloId}?entidadId=${entidadId}&uoId=${body.uoId}&personaEvaluadorId=${body.personaEvaluadorId}&detalleUoEvaluadorId=${body.evaluadoDetalleUoId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }

  listHistorialReuniones(body: ReunionHistory): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/reunion/listado/evaluado?detalleUoId=${body.detalleUoId}&cicloId=${body.cicloId}&evaluadoDetalleUoId=${body.evaluadoDetalleUoId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }

  listHistorialReunionesEvaluados(body: ReunionHistory): Observable<any> {
    const personaId = this.authService.getCurrentUserValue.personaId;
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    console.log(
      'this.authService.getCurrentUserValue.personaId:' +
        this.authService.getCurrentUserValue.personaId
    );
    const url = `${Const.API_PLANIFICACION}v1/reunion/listado/historial/evaluado?personaId=${personaId}&cicloId=${body.cicloId}&entidadId=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (response.status.success == false) {
          return response.status.error.messages[0];
        }
        return response.payload;
      })
    );
  }

  deleteReunion(reunionId: any): Observable<boolean> {
    const url = `${Const.API_PLANIFICACION}v1/reunion/eliminar/${reunionId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          return false;
        }
      })
    );
  }

  cancelarReunion(reunionId: any): Observable<boolean> {
    const url = `${Const.API_PLANIFICACION}v1/reunion/cancelar/${reunionId}`;
    return this.http.put(url, {}).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          return false;
        }
      })
    );
  }

  agregarReunion(datos: Reuniones): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/crear/reunion`;
    const body = { payload: datos };
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  notificarReunion(datos: Reuniones): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/reunion/reenviarcorreo`;
    const body = { payload: datos };
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }


  editarReunion(datos: Reuniones): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/editar/reunion`;
    const body = { payload: datos };
    return this.http.put(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
}

interface Reunion {
  cicloId: string;
  uoId: string;
  personaEvaluadorId: string;
  evaluadoDetalleUoId: string;
}

interface ReunionHistory {
  detalleUoId: string;
  cicloId: string;
  evaluadoDetalleUoId: string;
  personaId?: string;
}
