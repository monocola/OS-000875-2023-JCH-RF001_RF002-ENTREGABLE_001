import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ReportesRepository } from 'src/app/@domain/repository/reportes.repository';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { requestFilter } from 'src/app/utils/general';

@Injectable({
  providedIn: 'root',
})
export class ReportesService extends ReportesRepository {
  constructor(private http: HttpClient) {
    super();
  }

  getCodigoConvocatoria(entidadId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/convocatoria/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getResumenConvocatoria(request: any): Observable<any> {
    const url = `${Const.API_REPORTE}v1/convocatorias/resumen`;
    return this.http.post(url,request).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
  }

  getPerfilesEntidad(entidadId: number): Observable<any> {
    const estadoRegistro = 1;
    const url = `${Const.API_CONVOCATORIA}v1/perfil/filter/${entidadId}?estado=${estadoRegistro}`;

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getPostulantes(entidadId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/convocatoria/listarNombresApellidosByEntidad/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
  }

  getRoles(): Observable<any> {
    const estadoRegistro = 1;
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/roles/query?estadoRegistro=${estadoRegistro}&aplicacionId=${aplicacionId}`;

    return this.http.get<any>(url).pipe(
      map((response) => {
        return response.payload.items;
      })
    );
  }

  getRegimen(entidadId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/regimen/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getModalidadIngreso(entidadId: number, regimenId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/maestra?entidad=${entidadId}&regimen=${regimenId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.lstMaestraEntidad;
      })
    );
  }

  getEstadoConvocatoria(): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/getEstadosConvocatoria`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }

  getDescarga(request: any, baseId: number): Observable<any> {
    const url = `${Const.API_REPORTE}v1/reporte/convocatoria/pdf/${baseId}`;
    const tramaEnvio = {
      traceId: {
        traceId: 'string',
      },
      payload: {
        codigo: request.codigo,
        regimen: request.regimen,
        modadlidad: request.modadlidad,
        tipo: request.tipo,
        condicion: request.condicion,
        gestor: request.gestor,
        coordinador: request.coordinador,
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
          return response.payload;
      })
    );
  }

  getResponsables(entidadId: number, rolId: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/persona?idEntidad=${entidadId}&idRol=${rolId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        return response.payload.items;
      })
    );
  }
  getListaConvocatoria(request: any): Observable<any> {
    const url = `${Const.API_REPORTE}v1/convocatorias/reporte`;
    return this.http.post(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getReporteServir (body: any): Observable<any> {
    const params = requestFilter({
      fechaIni: body.fechaIni,
      fechaFin: body.fechaIni,
      entidadId: body.entidadId,
      regimenId: body.regimenId,
      etapaId: body.etapaId,
      estadoId: body.estadoId,
      departamentoId: body.departamentoId,
      rol: body.rol,
      responsableId: body.responsableId,
    });

    const url = `${Const.API_REPORTE}v1/convocatoria/reporte/servir?${params}`;

    console.log (url);

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.convocatorias;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
}
