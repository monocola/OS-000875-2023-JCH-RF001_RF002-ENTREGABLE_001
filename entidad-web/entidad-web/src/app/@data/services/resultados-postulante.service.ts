import { Injectable } from '@angular/core';
import { ResultadosPostulanteRepository } from '../../@domain/repository/resultados-postulante.repository';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { Const } from './const';
import { ResponseRequest } from '../model/reponse-request';
import { map } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})
export class ResultadosPostulanteService extends ResultadosPostulanteRepository {

  constructor(private http: HttpClient) { super(); }

  listarFormacionBasicaSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/formacionBasica/postulante/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarFormacionSuperiorSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/formacionSuperior/postulante/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarIdiomasSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/getIdiomas/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarOfimaticaSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/getOfimatica/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarEspecializacionSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/especializacion/postulante/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarExperienciaSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/experiencia/postulante/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  obtenerDatosPostulanteSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/getInformacionPostulante/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarOtrosRequisitosSeleccion(convocatoriaPostulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/otrosRequisitos/postulante/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarDeclaraJuradasSeleccion(convocatoriaId: any, postulanteId: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/declaracionJurada/${convocatoriaId}/${postulanteId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  listarFormacionAcademica(idPerfil: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/formacionAcademica/${idPerfil}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response.status.success) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }

  descargarCV(convocatoriaPostulante: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/descarga/cv/pdf/${convocatoriaPostulante}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (response) {
            return response.payload;
          } else {
            throw new Error(response.status.error.messages[0]).message;
          }
        }
      )
    );
  }
}
