import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { GraficosRepository } from 'src/app/@domain/repository/graficos.repository';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';

@Injectable({
  providedIn: 'root',
})
export class GraficosService extends GraficosRepository {
  responseError = {
    trace: { traceId: null },
    status: {
      success: null,
      error: { code: '', httpCode: '', messages: null },
    },
    payload: null,
  };

  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  downloadExcelGestor(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_REPORTE}v1/gestor/download`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  downloadExcelCoordinador(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_REPORTE}v1/coordinador/download`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  downloadExcelAdmin(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_REPORTE}v1/adminEntidad/download`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  downloadExcelServir(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_REPORTE}v1/servir/download`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  cabecera(
    rol: string,
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/informaciongeneral/${rol}/${entidadId}?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (estado !== null) {
      url = url + `&estado=${estado}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  graficoBarrasPuestos(
    rol: string,
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/puestostotales${rol}/${entidadId}?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (estado !== null) {
      url = url + `&estado=${estado}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoDonutPuestos(
    rol: string,
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/puestosRegimen${rol}?entidad=${entidadId}&fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (estado !== null) {
      url = url + `&estado=${estado}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  graficoDonutEstadoPostulante(
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/seguimientopostulante/${entidadId}?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (estado !== null) {
      url = url + `&estado=${estado}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  graficoBarrasPerfiles(
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/rankingperfiles/${
      entidadId === null ? 0 : entidadId
    }?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (estado !== null) {
      url = url + `&estado=${estado}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoDonutEstadoBase(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/baseEstado?entidad=${entidadId}&fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoBarraEstadoConvocatoria(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/convocatoriaEstado?entidad=${entidadId}&fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoDonutGestores(
    condicion: string,
    estado: number,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/usuarios/coordinador/${entidadId}?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (estado !== null) {
      url = url + `&estado=${estado}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  EstadoBasesByGestor(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/procesogestor/${entidadId}?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  cabeceraServir(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    let url = `${Const.API_REPORTE}v1/convocatoria/informaciongeneral/servir?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoLineEntidadesServir(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    let url = `${Const.API_REPORTE}v1/convocatoria/convocatoriaEntidad?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoRegimenServir(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    let url = `${Const.API_REPORTE}v1/convocatoria/convocatoriaRegimen?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoBarraPromedioConvocatoria(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    let url = `${Const.API_REPORTE}v1/convocatoria/promedioConvocatoria?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;

    if (entidadId !== null) {
      url = url + `&entidad=${entidadId}`;
    }
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoUsuariosByEntidad(
    condicion: string,
    fechaIni: string,
    fechaFin: string
  ): Observable<any> {
    let url = `${Const.API_REPORTE}v1/convocatoria/usuarios?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoUsuariosByConvDifusion(
    condicion: string,
    fechaIni: string,
    fechaFin: string,
    entidadId: number
  ): Observable<any> {
    let url = `${Const.API_REPORTE}v1/convocatoria/usuarios/difusion?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (entidadId !== null) {
      url = url + `&entidadId=${entidadId}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
  graficoUsuariosByConvEstados(
    condicion: string,
    fechaIni: string,
    fechaFin: string,
    entidadId: number
  ): Observable<any> {
    let url = `${Const.API_REPORTE}v1/convocatoria/usuarios/etapas?fechaIni=${fechaIni}&fechaFin=${fechaFin}`;
    if (condicion !== null) {
      url = url + `&condicion=${condicion}`;
    }
    if (entidadId !== null) {
      url = url + `&entidadId=${entidadId}`;
    }

    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
}
