import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs/operators';
import { ReporteDetalleConvocatoriaRepository } from 'src/app/@domain/repository/reporte-detalle-convocatoria.repository';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';


@Injectable({
  providedIn: 'root',
})

export class ReporteDetalleConvocatoriaService extends ReporteDetalleConvocatoriaRepository {
  constructor(
    private http: HttpClient,
  ) {
    super();
  }


  getListaDetalleConvocatoria(convocatoriaId: number) {

    const url = `${Const.API_CONVOCATORIA}v1/convocatoria/info/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );

  }

  getPerfilPuestoDetallePorConvocatoria(baseId: number) {
    const url = `${Const.API_SELECCION}v1/convocatoria/listarPerfilPuestoDetallebyConvocatoria/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getVacantesEnEtapaActual(baseId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas/etapaActual/${baseId}`;
    return this.http.get(url).pipe(
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
