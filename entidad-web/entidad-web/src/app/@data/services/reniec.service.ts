import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ReniecRepository } from 'src/app/@domain/repository/reniec.repository';
import { Observable } from 'rxjs';
import { catchError, map, timeout } from 'rxjs/operators';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class ReniecService implements ReniecRepository {
  constructor(private http: HttpClient) {}

  getPersonInfo(document: string, typeDoc?: number): Observable<any> {
    if (typeDoc == null) {
      typeDoc = document.length === 8 ? 1 : 4;
    }
    const url =
      Const.API_PERSONA +
      `v1/personas/documentoQuery?tipoDocumento=${typeDoc}&numeroDocumento=${document}`;
    const headers = new HttpHeaders().set('Content-Type', 'application/json');
    return this.http
      .get<any>(url, { headers })
      .pipe(
        map((response) => {
          console.log (response);

          if (!response.payload.personaNatural && typeDoc === 1) {
            throw new Error('1').message; // Documento DNI inexistente
          } else {
            if (!response.payload.personaNatural) {
              return true; // Persona con CE sin registro en API Persona
            } else {
              const persona = Object.assign(
                {},
                response.payload.personaNatural
              );
              response.payload.personaNatural.nombreCompleto = `${
                persona.nombres
              } ${persona.apellidoPaterno} ${
                persona.apellidoMaterno || ''
              }`.trim();
              return response.payload.personaNatural; // CE de ApiPersona o DNI de Reniec
            }
          }
        }),
        timeout(5000),
        catchError((e) => {
          if (e === '1') {
            throw new Error('El DNI ingresado no existe').message;
          } else {
            throw new Error(
              'El servicio de RENIEC no está disponible, intente más tarde'
            ).message;
          }
        })
      );
  }
}
