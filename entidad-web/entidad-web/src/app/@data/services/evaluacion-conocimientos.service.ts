import { HttpClient, HttpEvent, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Observable } from 'rxjs';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { Const } from './const';
import { catchError, map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { ToastService } from '../../@presentation/@common-components/toast';
import { armarPayload, Utils } from 'src/app/utils/utils';
@Injectable({
  providedIn: 'root',
})
export class EvaluacionConocimientosService extends EvaluacionConocimientosRepository {
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

  listarCategorias(cantidad: number, categoria: string): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        cantidad: cantidad,
        categoria: categoria,
        entidadId: entidadId,
      },
    };
    const url = `${Const.API_EVALUACION}v1/listarBancoPreguntas`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.preguntas;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  deleteCategoria(categoriaId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/categoria/eliminar/${categoriaId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  deleteExamen(examenId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/examen/eliminar/${examenId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarExamenes(
    cantidad: number,
    examen: string,
    convocatoriaId: number,
    perfilId: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidadId: entidadId,
        cantidad: cantidad,
        examen: examen,
        convocatoria: convocatoriaId,
        perfil: perfilId,
      },
    };
    const url = `${Const.API_EVALUACION}v1/listarExamenes`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.examenes;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarEvaluaciones(
    cantidad: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_EVALUACION}v1/resumen/programacion/${cantidad}/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        }
      })
    );
  }

  comboConvocatorias(): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_EVALUACION}v1/examen/convocatorias/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.lista;
        }
      })
    );
  }
  comboPerfiles(convocatoriaId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/examen/perfiles/${convocatoriaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.lista;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  guardarCategoria(categoriaId: number, descripcion: string): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        categoriaId: categoriaId,
        descripcion: descripcion,
        entidadId: entidadId,
      },
    };
    const url = `${Const.API_EVALUACION}v1/categoria/guardarCategoria`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  buscarBandejaCategoria(
    cantidad: number,
    descripcion: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        cantidad: cantidad,
        descripcion: descripcion,
        entidadId: entidadId,
      },
    };
    const url = `${Const.API_EVALUACION}v1/categoria/bandeja`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarTipoDuraciones(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/preguntas/combo/duracion`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.cmbDuracion;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  guardarActualizarPregunta(
    idPregunta: number,
    idDuracion: number,
    idCategoria: number,
    descPregunta: string,
    imagenUrl: string,
    tipoPregunta: number,
    explicacionOpc: string,
    alternativas: any,
    puntos: number,
    rangoIncremental: number
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        idPregunta: idPregunta,
        idDuracion: idDuracion,
        idCategoria: idCategoria,
        descPregunta: descPregunta,
        imagenUrl: imagenUrl,
        tipoPregunta: +tipoPregunta,
        explicacionOpc: explicacionOpc,
        alternativas: alternativas,
        puntos: +puntos,
        rangoIncremental: +rangoIncremental === 0 ? null : +rangoIncremental
      },
    };
    const url = `${Const.API_EVALUACION}v1/guardarActualizarPregunta`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response;
      }),
      catchError((err, caught) => {
        this.toast.showToast(
          'Ocurrió un error al guardar : ' + err.messages[0],
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  obtenerDetallePregunta(idPregunta: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/preguntaid/` + idPregunta;
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

  fileUpload(
    fileBase64: string,
    fileName: string,
    extension: string,
    observacion: string,
    path: string
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        fileBase64: fileBase64,
        fileName: fileName,
        extension: extension,
        observacion: observacion,
        path: path,
      },
    };
    const url = `${Const.API_MAESTRA}v2/file/upload`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response;
        } else {
          this.toast.showToast(
            response.status.error.messages[0],
            'danger',
            'Atención'
          );
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        let mensaje: any;
        if (
          err.messages[0] ===
          'Campo FileName debe tener una longitud minima de 5 caracteres'
        ) {
          mensaje =
            'El nombre de la imagen debe tener una longitud minima de 5 caracteres';
        } else if (
          err.messages[0] === 'Campo FileName sólo acepta números y letras'
        ) {
          mensaje =
            'El nombre de la imagene sólo debe contener números y letras';
        } else {
          mensaje = err.messages[0];
        }
        this.toast.showToast(mensaje, 'danger', 'Atención');
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }
  listarPreguntas(
    categoria: number,
    descripcion: string,
    tipo: string
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        categoria: categoria,
        descripcion: descripcion,
        tipo: tipo,
      },
    };
    const url = `${Const.API_EVALUACION}v1/preguntas/listarPreguntas`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.preguntas;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  listarTipoPregunta(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/preguntas/combo/tipopregunta`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.cmbDuracion;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  deletePreguntaDeCategoria(idPregunta: number): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        idPregunta: idPregunta,
      },
    };
    const url = `${Const.API_EVALUACION}v1/pregunta/eliminar`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  listarPreguntasPorCategoria(
    categoriaId: number,
    idExamen: number
  ): Observable<any> {
    const url =
      `${Const.API_EVALUACION}v1/preguntas/` + categoriaId + '/' + idExamen;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err) => {
        this.toast.showToast(
          'Ocurrió un error al obtener las preguntas por categoría',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  maxCantidadRespuestas(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/preguntas/maxresp`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        this.toast.showToast('Ocurrió un error', 'danger', 'Atención');
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  guardarActualizarExamen(
    idExamen: number,
    nombreExamen: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        examenId: idExamen,
        nombreExamen: nombreExamen,
        entidadId: entidadId,
      },
    };
    const url = `${Const.API_EVALUACION}v1/examen`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      }),
      catchError((err) => {
        this.toast.showToast(
          'Ocurrió un error al guardar : ' + err.messages[0],
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }
  getDetalleExamen(examenId: number): Observable<any> {
    const url =
      `${Const.API_EVALUACION}v1/listarExamenConocimiento/` + examenId;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err) => {
        this.toast.showToast(
          'Ocurrió un error al obtener el Detalle del Examen',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  guardarActualizarDetalleExamen(payloadArray: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: payloadArray,
    };
    const url = `${Const.API_EVALUACION}v1/detalleExamen`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        this.toast.showToast(
          'Ocurrió un error al guardar examen',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }
  eliminarExamenDetalle(examenDetalleId: any): Observable<any> {
    const url =
      `${Const.API_EVALUACION}v1/pregunta/eliminar/` + examenDetalleId;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err) => {
        this.toast.showToast(
          'Ocurrió un error al eliminar Detalle Examen',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  listarModalidadEvaluacion(): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/programacion/combo/modalidad`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.cmbDuracion;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  listarExamenesProgramacion(): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;

    const url = `${Const.API_EVALUACION}v1/examen/listar/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.lista;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  listarProgramaciones(
    convocatoria: number,
    examen: number,
    modalidad: number,
    perfil: number,
    grupo: number
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidad: entidadId,
        convocatoria: convocatoria,
        examen: examen,
        modalidad: modalidad,
        perfil: perfil,
        grupo: grupo,
      },
    };
    const url = `${Const.API_EVALUACION}v1/programaciones`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.programaciones;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  eliminarProgramacionByConvocatoria(
    convocatoriaId: number,
    perfilId: number
  ): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/programacion/eliminar/${convocatoriaId}/${perfilId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  listarGruposByConvocatoriaPerfil(
    convocatoriaId: number,
    perfilId: number
  ): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/programacion/perfiles/${convocatoriaId}/${perfilId}`;
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
  eliminarGrupo(programacionId: any): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/programacion/grupo/eliminar/${programacionId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  generarExamenPdf(idProgramacion: number): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        idProgramacion: idProgramacion,
      },
    };
    const url = `${Const.API_EVALUACION}v1/examen/pdf`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        this.toast.showToast(
          'Ocurrió un error al generar el PDF del examen',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }
  downloadTemplateExcel(categoriaId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/preguntayrespuesta/downloadFormat?categoria=${categoriaId}`;
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
  listarSedes(): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;

    const url = `${Const.API_EVALUACION}v1/sede/listar/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.listaSede;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        this.toast.showToast(
          'Ocurrió un error al obtener lista de sedes',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }

  getAllPersonasCuentaEntidad(idRol: number): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/persona?idEntidad=${entidadId}&idRol=${idRol}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        return response.payload.items;
      })
    );
  }

  registrarProgramacion(
    evaluadorId: number,
    modalidadId: number,
    convocatoriaId: number,
    cantidadPostulantes: number,
    nombreGrupo: string,
    fechaInicioExamen: string,
    fechaFinExamen: string,
    linkEvaluacion: string,
    examenId: number,
    sedeId: number,
    indicacionesAdicionales: string,
    perfilId: number,
    tipoExamen: string,
    programacionId: number,
    nombreSede: string,
    ubigeoId: string,
    lugarEvaluacion: string,
    referenciaLugar: string
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        evaluadorId: evaluadorId,
        modalidadId: modalidadId,
        convocatoriaId: convocatoriaId,
        cantidadPostulantes: cantidadPostulantes,
        nombreGrupo: nombreGrupo,
        fechaInicioExamen: Utils.formatFechaString(
          fechaInicioExamen,
          'YYYY-MM-DD HH:mm:ss'
        ),
        fechaFinExamen: Utils.formatFechaString(
          fechaFinExamen,
          'YYYY-MM-DD HH:mm:ss'
        ),
        linkEvaluacion: linkEvaluacion,
        entidadId: entidadId,
        examenId: examenId,
        sedeId: sedeId,
        indicacionesAdicionales: indicacionesAdicionales,
        perfilId: perfilId,
        tipoExamen: tipoExamen,
        programacionId: programacionId,
        nombreSede: nombreSede,
        ubigeoId: ubigeoId,
        lugarEvaluacion: lugarEvaluacion,
        referenciaLugar: referenciaLugar,
      },
    };

    const url = `${Const.API_EVALUACION}v1/programacion/crearGrupo`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        this.toast.showToast(
          'Ocurrió un error al guardar Grupo',
          'danger',
          'Atención'
        );
        throw new Error(JSON.stringify(err)).message;
      })
    );
  }
  examenVirtualCabecera(programacionId: number): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_EVALUACION}v1/programacion/examen/virtual/${entidadId}/${programacionId}`;
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
  PreguntasByExamen(examenId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/preguntas/listarPreguntas/${examenId}`;
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

  ObtenerAplicacionPorAbreviatura(abreviatura: string): Observable<any> {
    const url = `${Const.API_SEGURIDAD}v1/aplicaciones/query?estado=1&abreviatura=${abreviatura}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items[0];
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        throw new Error(err).message;
      })
    );
  }

  ObtenerRolesPorAplicacion(
    aplicacionId: number,
    nombreRol: string
  ): Observable<any> {
    const url = `${Const.API_SEGURIDAD}v1/roles/query?aplicacionId=${aplicacionId}&nombreRol=${nombreRol}&estadoRegistro=1`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items[0];
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((err, caught) => {
        throw new Error(err).message;
      })
    );
  }

  validaPlantillaPreguntasRespuestas(multipartData: FormData, categoriaId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/validarPreguntasRespuestas/${categoriaId}`;
    return this.http.post(url,multipartData).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
   }

  guardarExamenPreguntasRespuestasMasiva(categoriaId: number, listaExamenPreguntasRespuestas: any): Observable<any> {
    let request = {listaExamenPreguntasRespuestas};
    const req = armarPayload(request);
    const url = `${Const.API_EVALUACION}v1/guardarExamenPreguntasRespuestasMasivo/${categoriaId}`;
    return this.http.post(url,req).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }
  eliminarPreguntasExcel(cabeceraId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/eliminarPreguntayAlternativas/${cabeceraId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
  envioCorreo(convocatoriaId: number, perfilId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/programacion/tosend/${convocatoriaId}/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          this.toast.showToast(
            response.status.error.messages[0],
            'danger',
            'Atención'
          );
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  buscarSeguimientoEvaluacion( tipoEvaluacion: number, baseId: number, filtros: any): Observable<any> {
    const request = armarPayload(filtros);
    const url = `${Const.API_SELECCION}v1/evaluacion/seguimiento/${tipoEvaluacion}/${baseId}`;
    return this.http.post(url,request).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          this.toast.showToast(
            response.status.error.messages[0],
            'danger',
            'Atención'
          );
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
   }

   listarFechasEtapa(etapaId: number, baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas/etapa/${etapaId}/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
   }

   getPlantillaPreguntasMasivas(): Observable<any>  {
    const url = `${Const.API_EVALUACION}v1/preguntas/examen/masiva`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
   }

   validaPlantillaPreguntas(multipartData: FormData, entidadId: number): Observable<any> {
    const url = `${Const.API_EVALUACION}v1/validarPreguntasExamen/${entidadId}`;
    return this.http.post(url,multipartData).pipe(
      map((response: ResponseRequest) => {
        return response.payload;
      })
    );
   }

   guardarExamenPreguntasMasiva(entidadId: number, listaExamenPreguntas: any): Observable<any> {
    let request = {listaExamenPreguntas};
    const req = armarPayload(request);
    const url = `${Const.API_EVALUACION}v1/guardarExamenPreguntasMasivo/${entidadId}`;
    return this.http.post(url,req).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        }
      })
    );
  }

  actualizarFlagSanciones(baseId, perfilId): Observable<any> {
    const url = `${Const.API_SELECCION}v1/postulacion/actualizarFlagSanciones/${baseId}/${perfilId}`;
    return this.http.post(url,"").pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          this.toast.showToast(
            response.status.error.messages[0],
            'danger',
            'Atención'
          );
        }
      }),
      catchError((err, caught) => {
        throw new Error(err).message;
      })
    );
  }


}
