import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { SeguimientoCronogramaRepository } from 'src/app/@domain/repository/seguimiento-cronograma.repository';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';


@Injectable({
  providedIn: 'root'
})
export class SeguimientoCronogramaService  extends SeguimientoCronogramaRepository {

  constructor(private http: HttpClient) {
     super();
   }

   getCronograma(): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}/v1/basecronograma/listar/6`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.basecronograma;
      })
    );
  }


}
