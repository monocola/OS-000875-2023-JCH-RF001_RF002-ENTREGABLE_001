import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { SunatRepository } from 'src/app/@domain/repository/sunat.repository';
import { Observable } from 'rxjs';
import { catchError, map, timeout } from 'rxjs/operators';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class SunatService implements SunatRepository {
  constructor(private http: HttpClient) {}

  getSunatInfo(ruc: string): Observable<any> {
    const url = `${Const.API_PERSONA}v1/personas/documentoQuery?tipoDocumento=6&numeroDocumento=${ruc}`;
    const headers = new HttpHeaders().set(
      'Content-Type',
      'application/json'
    );
    return this.http
      .get<any>(url, { headers })
      .pipe(
        map((response) => {
          if (response.payload) {
            if (response.payload.personaJuridica) {
              return response.payload;
            } else {
              return {
                code: 1,
                message: 'El RUC ingresado no existe',
              };
            }
          } else {
            return { code: 2, message: 'El RUC ingresado no existe' };
          }
        }),
        timeout(5000),
        catchError((e) => {
          throw new Error(
            'El servicio de SUNAT no está disponible, intente más tarde'
          ).message;
        })
      );
  }
}
