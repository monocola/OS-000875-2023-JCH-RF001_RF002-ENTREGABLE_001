import { Injectable } from '@angular/core';
import { MaestraParametroRepository } from '../../@domain/repository/maestra-parametro.repository';
import { HttpClient } from '@angular/common/http';
import { Const } from './const';
import { Observable } from 'rxjs';
import { MaestraParametro } from '../model/maestra-parametro';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';


@Injectable({
  providedIn: 'root'
})
export class MaestraParametroService implements MaestraParametroRepository {
parametro: MaestraParametro[] = [];

  constructor(
    private http: HttpClient
  ) { }

  getMaestraParametro(tipo_parametro: string): Observable<MaestraParametro[]> {
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${tipo_parametro}/parametros`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          return this.parametro = response.payload.items;
        }
      )
    );
  }



}
