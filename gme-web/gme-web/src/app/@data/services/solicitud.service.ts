import { Injectable } from '@angular/core';
import { SolicitudRepository } from '../../@domain/repository/solicitud.repository';
import { Observable } from 'rxjs/Observable';
import { Const } from './const';
import { map } from 'rxjs/operators';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root',
})
export class SolicitudService implements SolicitudRepository {

  constructor(
    private http: HttpClient,
  ) { }

  getRuc(ruc: string): Observable<any> {
    let tipoRuc = 6;
    const url = `${Const.API_ENTIDAD}v1/externa/${tipoRuc}/${ruc}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response;
      })
    ); 
  }

  getSolicitudExtId(Id: number): Observable<any> {
    let url = `${Const.API_ENTIDAD}v1/externa/solicitudExt/${Id}`;
    return this.http.get(url).pipe(
      map((response: any) => {
         return response.payload;
      })
    );
  }
}
