import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ReportePostulanteRepository } from 'src/app/@domain/repository/reporte-postulante-repository';
import { armarPayload } from 'src/app/utils/utils';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';


@Injectable({
    providedIn: 'root',
})

export class ReportePostulanteService extends ReportePostulanteRepository {


    constructor(
        private http: HttpClient,
    ) {
        super();
    }

    buscarReportePostulantes(payload: any): Observable<any> {
        const url = `${Const.API_REPORTE}v1/reportes/listarBandejaPostulantes`;
        let request = armarPayload(payload);
        return this.http.post(url, request).pipe(
            map((response: ResponseRequest) => {
                if (response.status.success) {
                    return response.payload;
                } else {
                    throw new Error(response.status.error.messages[0]).message;
                }
            })
        );
    }

}
