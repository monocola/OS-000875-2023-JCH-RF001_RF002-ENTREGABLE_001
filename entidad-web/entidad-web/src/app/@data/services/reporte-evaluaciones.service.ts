import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ReporteEvaluacionesRepository } from 'src/app/@domain/repository/reporte-evaluaciones.repository';
import { armarPayload } from 'src/app/utils/utils';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';


@Injectable({
  providedIn: 'root',
})

export class ReporteEvaluacionesService extends ReporteEvaluacionesRepository {


  constructor(
    private http: HttpClient,
  ) {
    super();
  }


  getListaEvaluaciones(baseId: number, entidadId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/listarTipoEvaluaciones/${baseId}/${entidadId}`;
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

  getCodigoConvocatoria(entidadId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/postulacion/convocatoria/${entidadId}`;
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

  getListaNombresPorConvocatoriaPerfil(convocatoriaId: number, perfilId: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/postulantes/${convocatoriaId}/${perfilId}`;
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

  getPerfilesPorConvocatoria(convocatoriaId: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/listaPerfiles/${convocatoriaId}`;
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

  getTipoDocumentos(): Observable<any> {
    const parametro = 'PER_TIPO_DOCUMENTO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }

      } )
    );
  }

  getIndicadoresPostulante(convocatoriaPostulId: number) {
    const url = `${Const.API_SELECCION}v1/evaluacion/postulante/indicador/${convocatoriaPostulId}`;
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

  getDataPuesto(baseId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/convocatoria/datosPuesto/${baseId}`;
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

  buscarReporteEvaluaciones(payload: any) {
    const url = `${Const.API_REPORTE}v1/evaluacion/reporte/bandeja`;
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

  getinfoevaluacionpostulante(convocatoriaId: number, perfilId: number, postulanteId: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/getInfoEvaluacionPostulante/${convocatoriaId}/${perfilId}/${postulanteId}`;
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


  getDescargaPDF(convocatoriaId: number, perfilId: number, postulanteId: number) {
    const url = `${Const.API_REPORTE}v1/reporte/evaluacionDetallePostulante/pdf/${convocatoriaId}/${perfilId}/${postulanteId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.payload) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

}
