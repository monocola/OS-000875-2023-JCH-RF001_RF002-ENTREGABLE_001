import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { ToastService } from '../../@presentation/@common-components/toast';
import { Const } from './const';
import { ResponseRequest } from '../model/reponse-request';
import { map } from 'rxjs/operators';
import { Observable } from 'rxjs/Observable';
import { CboNivel } from '../model/evaluacion-curricular/evaluacion';
import { armarPayload } from 'src/app/utils/utils';

@Injectable({
  providedIn: 'root',
})
export class EvaluacionCurricularService extends EvaluacionCurricularRepository {
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
    private authenticationRepository: AuthenticationRepository,
    private toast: ToastService
  ) {
    super();
  }

  getEstadosConvocatoria() {
    const url = `${Const.API_SELECCION}v1/postulacion/getEstadosConvocatoria`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getEvaluacionFiltro(entidadId: number, request: any) {
    const url = `${Const.API_SELECCION}v1/evaluacion/filter/${entidadId}`;
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

  listarConvocatoriasPostulante(request: any) {
    const url = `${Const.API_SELECCION}v1/evaluacion/listaEvaluacionCurricular`;
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

  listarBonoConvocatoria(convocatoriaPostulanteId: any) {
    const url = `${Const.API_SELECCION}v1/bonificacion/listar/${convocatoriaPostulanteId}`;
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

  getFlagTipoBono(resumenId: number, convocatoriaPostulanteId: number) {
    const url = `${Const.API_SELECCION}v1/curricular/flagbono/${resumenId}/${convocatoriaPostulanteId}`;
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

  listarEstadoEvaluacionCurricular(flag: any) {
    const url = `${Const.API_SELECCION}v1/evaluacion/listaEstadoEvaluacionCurricular/${flag}`;
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

  listarPerfilPuestobyConvocatoria(idBase: any) {
    const url = `${Const.API_SELECCION}v1/evaluacion/listarPerfilPuestobyConvocatoria/${idBase}`;
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

  experiencialLaboralByPerfil(perfilId: any, convocatoriaId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/experiencialLaboralByPerfil/${perfilId}/${convocatoriaId}`;
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

  investigacionByPerfil(perfilId: any, convocatoriaId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/investigacion/${perfilId}/${convocatoriaId}`;
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

  listarConvocatoriaCarreras(perfilId: number, convocatoriaId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/carreras/${perfilId}/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarConvocatoriaEspecialidades(perfilId: number, convocatoriaId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/configuracion/especialidad/${perfilId}/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarPostulanteFormacionAcademica(
    convocatoriaId: number,
    postulanteId: number,
    perfilId: number
  ) {
    const url = `${Const.API_SELECCION}v1/postulacion/formacionAcademica/${convocatoriaId}/${postulanteId}/${perfilId}`;
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

  listarPostulanteExperiencia(
    convocatoriaId: number,
    postulanteId: number,
    perfilId: number
  ) {
    const url = `${Const.API_SELECCION}v1/postulacion/experiencia/${convocatoriaId}/${postulanteId}/${perfilId}`;
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

  listarInvestigacionPostulante(
    convocatoriaId: number,
    postulanteId: number,
    perfilId: number
  ) {
    const url = `${Const.API_SELECCION}v1/postulacion/investigacion/${convocatoriaId}/${postulanteId}/${perfilId}`;
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

  listarDeclaracionesJuradas(convocatoriaId: number, postulanteId: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/declaracionJurada/${convocatoriaId}/${postulanteId}`;
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

  listarOtrosRequisitosPostulante(convocatoriaPostulanteId: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/listarOtrosRequisitosPostulante/${convocatoriaPostulanteId}`;
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

  ListaConfigOtrosrequisitos(idPerfil: number) {
    const url = `${Const.API_CONVOCATORIA}v1/ListaConfigOtrosrequisitos/${idPerfil}`;
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
 
  documentoSustento(request: any) {
    const url = `${Const.API_POSTULANTE}v1/documentoSustento`;
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

  actualizarSeccionEvaluacionFormac(seccionFormacionId: number, request: any) {
    const url = `${Const.API_SELECCION}v1/seccionEvaluacionFormac/estado/${seccionFormacionId}`;
    return this.http.put(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  actualizarSeccionEvaluacionExp(seccionExperienciaId: number, request: any) {
    const url = `${Const.API_SELECCION}v1/seccionEvaluacionExp/estado/${seccionExperienciaId}`;
    return this.http.put(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarPuntajeEvaluacionCurricular(convocatoriaPostulanteId: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/puntaje/${convocatoriaPostulanteId}`;
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

  listarUrlDocumentos(convocatoriaPostulanteId: number): Observable<any> {
    const url = `${Const.API_SELECCION}v1/documento/bonificacion/${convocatoriaPostulanteId}`;
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

  getNivelBonificacion(
    convocatoriaPostulanteId: number
  ): Observable<CboNivel[]> {
    const url = `${Const.API_SELECCION}v1/evaluacion/curricular/listaNivelBonificaciones/${convocatoriaPostulanteId}`;
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

  guardarBonificacion(resumenId: number, item: any): Observable<any> {
    const url = `${Const.API_SELECCION}v1/registrarValidarBonificacion/postulante/${resumenId}`;
    let request = armarPayload<any>(item);
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

  setearBonificacion(
    resumenId: number,
    tipoBonificacion: number
  ): Observable<any> {
    const url = `${Const.API_SELECCION}v1/evaluacion/curricular/bonificacionPostulante/${resumenId}/${tipoBonificacion}`;
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

  evaluacionResumen(convocatoriaPostulanteId: number) {
    const url = `${Const.API_SELECCION}v1/evaluacion/resumen/${convocatoriaPostulanteId}`;
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

  getPDFEvaluacionCurricular(
    convocatoriaPostulanteId: number
  ): Observable<any> {
    const url = `${Const.API_SELECCION}v1/reporte/evaluacionCurricular/pdf/${convocatoriaPostulanteId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  actualizarRedereci(convocatoriaPostulante: number, request: any) {
    const url = `${Const.API_SELECCION}v1/convocatoria/estadoRedereci/${convocatoriaPostulante}`;
    return this.http.put(url, request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarEvaluacionInvestigacion(convocatoriaPostulante: number) {
    const url = `${Const.API_SELECCION}v1/postulacion/evaluacioninvestigacion/${convocatoriaPostulante}`;
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

  actualizarEvaluacionInvestigacion(lstInvestigacionPostulante:any) {
    const url = `${Const.API_SELECCION}v1/seccionInvestigacionPublica`;
    const req = armarPayload(lstInvestigacionPostulante);
    console.log("Request para actualizar investigacion", req);
    return this.http.put(url, req).pipe(
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
