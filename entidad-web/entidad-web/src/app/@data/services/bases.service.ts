import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Observable, throwError } from 'rxjs';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { Const } from './const';
import { catchError, concatMap, map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { requestFilter } from 'src/app/utils/general';
import { Base } from '../model/base';
import moment from 'moment';
import { UpdateEstado } from '../model/bases/update.estado';
import { RequestGeneric } from '../model/generic/request.generic';
import { ResponseGeneric } from '../model/generic/response.generic';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';

@Injectable({
  providedIn: 'root',
})
export class BasesService extends BasesRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository,
    private maestraRepository: MaestraRepository
  ) {
    super();
  }
  infoEva: DetalleMaestra[] = [];
  getBases(body: any): Observable<Base[]> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const fechaIni = body?.fecha?.start || null;
    const fechaFin = body?.fecha?.end ? body?.fecha?.end : fechaIni;

    const fecIniFormatted = fechaIni
      ? moment(fechaIni).format('yyyy-MM-DD')
      : null;

    const fecFinFormatted = fechaFin
      ? moment(fechaFin).format('yyyy-MM-DD')
      : null;

    const params = requestFilter({
      entidadId,
      codigoConvocatoria: body?.codigo,
      nombrePerfil: body?.nombrePerfil,
      fechaIni: fecIniFormatted,
      fechaFin: fecFinFormatted,
      etapaId: body?.estado,
      regimenId: body?.regimen,
      modalidadId: body?.modalidad,
      tipoId: body?.tipo,
      practicaId: body?.tipoPractica,
      condicionId: body?.condicion,
    });
    const url = `${Const.API_CONVOCATORIA}v1/bases/filter?${params}`;
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

  getBasesRevision(body: any): Observable<Base[]> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const etapaId = Const.ETA_BASE_POR_REVISAR;

    const params = requestFilter({
      entidadId,
      etapaId,
    });
    const url = `${Const.API_CONVOCATORIA}v1/bases/filter?${params}`;
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

  deleteBase(baseId: number): Observable<boolean> {
    const url = `${Const.API_CONVOCATORIA}v1/base/estado?baseId=${baseId}&estado=0`;
    return this.http.delete(url, {}).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getDataStep1(baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/concurso?baseId=${baseId}`;
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

  getPDFInforme(informeDetalleId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/informedetalle/pdf/${informeDetalleId}`;
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

  getPDF(
    idInforme: number,
    idTipoInformacion: number,
    baseId: number
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        idInforme: idInforme,
        idTipoInformacion: idTipoInformacion,
        baseId: baseId,
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/pdf/generar/informe`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.pdfByte;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getPDFPerfil(perfilId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/basecronograma/perfiles/pdf/${perfilId}`;
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

  getDataStep2(baseId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/vacante/${baseId}`;
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

  getIndividualPerfilBase(basePerfilId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/perfil/${basePerfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.basePerfil;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  deleteIndividualPerfilBase(basePerfilId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/vacante/estado?basePerfilId=${basePerfilId}&estado=0`;
    return this.http.put(url, {}).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getDataStep3(baseId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/requisitoGeneral?baseId=${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      }),
      catchError((e) => {
        return throwError(e);
      })
    );
  }

  getDataStep4(baseId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/evaluacion/${baseId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (response.status.success) {
          if (response.payload.informeDetalleId === null) return null;
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getDataStep6(baseId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/etapa6/${baseId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (response.status.success) {
          if (response.payload.informes === null) return null;
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveOrUpdateStep1(body: any, baseId: any): Observable<any> {
    const user = this.authenticationRepository.getCurrentUserValue;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        datoConcursoId: body.datoConcursoId || null,
        nombre: body.nombreConcurso || null,
        objetivo: body.objetivo || null,
        organoResponsableId: body.organo.organigramaId || null,
        unidadOrganicaId: body.unidadOrganica.organigramaId || null,
        organoEncargadoId: body.organoEncargado || null,
        correo: body.correo || null,
        baseId: baseId || null,
        nroVacantes: body.numeroVacantes || null,
        telefono: body.telefono || null,
        anexo: body.anexo || null,
        informeDetalleId: body.baseSeleccionada[0].value || null,
        informeEspecificaId:
          body.baseSeleccionadaEsp == null
            ? null
            : body.baseSeleccionadaEsp.length === 0
              ? null
              : body.baseSeleccionadaEsp[0].value || null,
        tipoPracticaId: body.tipoPractica || null,
        baseRegimenId: body.regimen || null,
        baseModalidadId: body.modalidad || null,
        baseTipoId: body.tipo || null,
        baseRolID: user.rolId,
        baseEspecialistaId: null,
        baseGestorId: user.personaId,
        baseEntidadId: user.entidadId,
        nombreEspecialista: null,
        nombreGestor: `${user.nombres} ${user.apellidoPaterno} ${user.apellidoMaterno || ''
          }`.trim(),
      },
    };
    let request = null;
    if (baseId) {
      const url = `${Const.API_CONVOCATORIA}v1/base/datoconcurso`;
      request = this.http.put(url, tramaEnvio);
    } else {
      const url = `${Const.API_CONVOCATORIA}v1/base`;
      request = this.http.post(url, tramaEnvio);
    }
    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.payload) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveOrUpdateStep2(
    body: any,
    baseId: any,
    basePerfilId: number,
    bodySede: any
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        basePerfilId: basePerfilId || null,
        baseId: baseId,
        perfilId: body.perfil.perfilId,
        jornadaLaboral: body.jornada,
        vacante: body.vacantes,
        remuneracion: body.remuneracion,
        indContratoIndeterminado: body.contratoIndeterminado ? 1 : 0,
        tiempoContrato: body.duracionContrato,
        condicionTrabajoId: body.condicion,
        modalidadContratoId: body.modalidadContrato || null,
        sedeId: body.sede.sedeId,
        sedeDireccion: body.sede.valueToShow,
        condicionEsencial: body.condicionesEscenciales,
        observaciones: body.observaciones,
        estado: 1,
        nombrePerfil: body.perfil.nombrePuesto,
        baseHorarioDTOList: this.setHorarios(body.horarios),
        depaId: bodySede[0].departamentoId,
        descDepa: bodySede[0].departamento,
        provId: bodySede[0].provinciaId,
        descProv: bodySede[0].provincia,
        distId: bodySede[0].distritoId,
        descDist: bodySede[0].distrito,
      },
    };
    let request = null;
    if (basePerfilId) {
      const url = `${Const.API_CONVOCATORIA}v1/base/perfil/${basePerfilId}`;
      request = this.http.put(url, tramaEnvio);
    } else {
      const url = `${Const.API_CONVOCATORIA}v1/base/perfil`;
      request = this.http.post(url, tramaEnvio);
    }
    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.payload) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  setHorarios(horarios: any[]) {
    const horariosFormatted = horarios.map((h) => {
      return {
        baseHorarioId: h.id,
        horaIni: h.horaInicio,
        horaFin: h.horaFin,
        frecuenciaId: h.diaId,
        estado: 1,
      };
    });
    return horariosFormatted;
  }

  saveOrUpdateStep3(
    body: any,
    baseId: any,
    requisitoId?: number
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        baseId: baseId,
        declaracionJuradaDTOList: this.setDeclaracionesJuradas(body),
      },
    };
    let request = null;
    if (requisitoId) {
      const url = `${Const.API_CONVOCATORIA}v1/base/requisitoGeneral`;
      request = this.http.put(url, tramaEnvio);
    } else {
      const url = `${Const.API_CONVOCATORIA}v1/base/requisitoGeneral`;
      request = this.http.post(url, tramaEnvio);
    }
    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.payload) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveOrUpdateStep4(
    baseId: number,
    observacion: string,
    params: any
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: {
        baseId: baseId,
        observacion: observacion,
        baseEvaluacionDetalleDTOList: params,
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/base/guardarEvaluacion`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  saveOrUpdateStep6(
    baseId: any,
    observaciones: string,
    informes: any
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: {
        baseId: baseId,
        observaciones: observaciones,
        informes: informes,
      },
    };
    
    const url = `${Const.API_CONVOCATORIA}v1/base/guardarInfoCompl`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  /*getEtapas(): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/basecronograma/etapas`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const lista = [];
        response.payload.maestraDetalle.forEach((element) => {
          lista.push({
            value: element.maeDetalleId,
            valueToShow: element.descripcion,
          });
        });
        return lista;
      })
    );
  }*/
  getEtapas(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        const lista = [];
        response.payload.maestraDetalles.forEach((element) => {
          if (element.estadoRegistro !== '0') {
            lista.push({
              value: element.maeDetalleId,
              valueToShow: element.descripcionCorta,
              codProg: element.codProg,
            });
          }
        });
        return lista;
      })
    );
  }

  getListaCronogramas(baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/basecronograma/listar/${baseId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.basecronograma;
      })
    );
  }

  getListaCronogramasV2(baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas/${baseId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.baseCronogramaDTOList;
      })
    );
  }

  getListaCronogramasHistorico(baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas/${baseId}?history=1`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.baseCronogramaDTOList;
      })
    );
  }

  guardarBaseCronograma(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_CONVOCATORIA}v1/basecronograma`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  guardarBaseCronogramaV2(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  eliminarCronograma(id: number): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: {
        estado: 0,
        actividadId: id,
      },
    };

    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas/actividad`;

    return this.http.put(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  editarCronograma(params: any, id: number): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };
    const url = `${Const.API_CONVOCATORIA}v1/basecronograma/${id}`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  editarCronogramaV2(params: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: '',
      },
      payload: params,
    };

    const url = `${Const.API_CONVOCATORIA}v1/basecronogramas`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  setDeclaracionesJuradas(body) {
    const arregloServirNew: any[] =
      body.declaracionesJuradasServir?.map((dj) => {
        return {
          declaracionId: dj.declaracionId || null,
          tipoId: dj.tipoId,
          orden: dj.orden,
          descripcion: dj.descripcion,
          estado: dj.estado,
          isServir: dj.isServir,
          idBase: dj.idBase,
        };
      }) || [];
    const decJurEntidad: any[] =
      body.declaraJuradaRequeridos?.map((dje) => {
        return {
          declaracionId: dje.datasend.declaracionId || null,
          tipoId: dje.datasend.maeDetalleEntidadId,
          orden: dje.datasend.orden,
          descripcion: dje.datasend.descripcion,
          estado: dje.datasend.estadoRegistro,
          isServir: '0',
          idBase: dje.datasend.idBase || null,
        };
      }) || [];
    const decToDelete: any[] =
      body.declaraJuradaRequeridosToDelete?.map((dje) => {
        return {
          declaracionId: dje.declaracionId,
          tipoId: dje.maeDetalleEntidadId,
          orden: dje.orden,
          descripcion: dje.descripcion,
          estado: '0',
          isServir: dje.isServir,
          idBase: dje.idBase || null,
        };
      }) || [];
    return arregloServirNew.concat(decJurEntidad, decToDelete);
  }

  getCriteriosDeEvaluacion(): Observable<any> {
    return this.obtenerParam().pipe(
      concatMap((param) => {
        return this.procesarCriteriosDeEvaluacion(param);
      }),
      catchError((e) => {
        throw e;
      })
    );
  }

  obtenerParam(): Observable<any> {
    return this.maestraRepository.getMaestraDetalleByCod('TIP_INF').pipe(
      map((maestraList) => {
        let tipoidd = maestraList.find((tipoInfo) => tipoInfo.codProg === '4');
        if (tipoidd != null) {
          return tipoidd.maeDetalleId;
        } else {
          throwError("Error al encontrar codprog = '4'");
        }
      })
    );
  }

  procesarCriteriosDeEvaluacion(tipoInformeId: any): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_CONVOCATORIA}v1/informedetalle/criteriosEvaluacion/${entidadId}/${tipoInformeId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const lista = [];
        response.payload.items.forEach((element) => {
          lista.push({
            value: element.informeDetalleId,
            description: element.titulo,
          });
        });
        return lista;
      })
    );
  }

  getListaDeEvaluaciones(baseId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/evaluacion/${baseId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (response.payload)
          return response.payload.baseEvaluacionDetalleDTOList;
      })
    );
  }
  /*getTiposDeInforme(): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/combo/tipoInforme`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const lista = [];
        response.payload.listaDetalles.forEach((element) => {
          lista.push({
            value: element.maeDetalleId,
            valueToShow: element.descripcionCorta,
          });
        });
        return lista;
      })
    );
  }*/
  getTiposDeInforme(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        console.log(response);
        const lista = [];
        response.payload.maestraDetalles.forEach((element) => {
          if (element.estadoRegistro !== '0') {
            lista.push({
              value: element.maeDetalleId,
              valueToShow: element.descripcionCorta,
            });
          }
        });
        return lista;
      })
    );
  }

  getTiposBonificacion(codigo_cabecera: string): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle?codcabecera=${codigo_cabecera}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        const lista = [];
        response.payload.maestraDetalles.forEach((element) => {
          if (element.estadoRegistro !== '0') {
            lista.push({
              value: element.maeDetalleId,
              valueToShow: element.descripcionCorta,
            });
          }
        });
        return lista;
      })
    );
  }

  getInformes(tipoInformeId: number): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_CONVOCATORIA}v1/informedetalle/criteriosEvaluacion/${entidadId}/${tipoInformeId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const lista = [];
        if (response.status.success) {
          response.payload.items.forEach((element) => {
            lista.push({
              value: element.informeDetalleId,
              description: element.titulo,
              tipoInformeId: tipoInformeId,
            });
          });
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
        return lista;
      })
    );
  }

  publicarBase(data: any): Observable<boolean> {
    const url = `${Const.API_CONVOCATORIA}v1/base/movimiento`;
    let body = new RequestGeneric<UpdateEstado>(data);
    return this.http
      .post<ResponseGeneric<void>>(url, body, { observe: 'response' })
      .pipe(
        map((response) => {
          if (response.body.status.success) {
            return true;
          } else {
            throw new Error(response.body.status.error.messages[0]);
          }
        })
      );
  }

  getMovimientosObs(baseId: number) {
    const url = `${Const.API_CONVOCATORIA}v1/base/movimiento/${baseId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        }
      })
    );
  }

  saveMovimientosObservaciones(
    oldEstado,
    newEstado,
    baseId: number,
    observaciones: any[]
  ): Observable<any> {
    const persona = JSON.parse (sessionStorage.getItem ("persona"));
    const url = `${Const.API_CONVOCATORIA}v1/base/movimiento`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        movimientoId: null,
        baseId: baseId,
        rolId: this.authenticationRepository.getCurrentUserValue.rolId,
        entidadId: this.authenticationRepository.getCurrentUserValue.entidadId,
        estadoOldId: oldEstado,
        estadoNewId: newEstado,
        coordinadorId: persona.personaId,
        nombreCoordinador: persona.nombreCompleto,
        observacionDTOList: observaciones,
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((response: any) => {
        if (response.status.success) {
          return true;
        } else {
          throw new Error(response.body.status.error.messages[0]);
        }
      })
    );
  }

  obtenerPdf(idBase: string): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/base/pdf/${idBase}`;
    return this.http
      .get<ResponseGeneric<void>>(url, { observe: 'response' })
      .pipe(
        map((response) => {
          if (response.status === 200) {
            return response.body.payload;
          } else {
            throw new Error(response.body.status.error.messages[0]);
          }
        })
      );
  }


  obtenerPdfNew(perfilId: string): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/pdf/${perfilId}`;
    //    const url = `${Const.API_CONVOCATORIA}v1/base/pdf/97`;

    return this.http
      .get<ResponseGeneric<void>>(url, { observe: 'response' })
      .pipe(
        map((response) => {
          if (response.status === 200) {
            return response.body.payload;
          } else {
            throw new Error(response.body.status.error.messages[0]);
          }
        })
      );
  }
}
