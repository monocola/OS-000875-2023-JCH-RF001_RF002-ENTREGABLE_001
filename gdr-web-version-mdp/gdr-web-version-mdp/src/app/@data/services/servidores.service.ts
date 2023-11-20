import { HttpClient, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ResponseRequest, Status } from '../model/reponse-request';
import { Const } from './const';
import { requestFilter } from '../../utils/general';
import { MaestraParametro } from '../model/maestra-parametro';
import { DatosPersonalesServidorCivil } from '../model/servidores-civiles';
import { PuestoUoServidorCivil } from '../model/puesto';
import { IParticipanteEvaluador } from '../model/participante';
import { PayloadMetas } from '../model/meta';
import { SidenavService } from './sidenav.service';
import { Util } from 'leaflet';
import { Utils } from 'src/app/utils/utils';

@Injectable({
  providedIn: 'root',
})
export class ServidoresService implements ServidoresRepository {

  constructor(
    private http: HttpClient,
    private authService: AuthenticationRepository,
    public sidenavService: SidenavService,

  ) {}

 /*  uploadFileMasivo(body: any, cicloId: any): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
     const url = `http://10.241.161.105:8088/planificacion/api/public/v1/servidorCivil/validaPlantillaSegmento?cicloId=4`;
  // const url = `${Const.API_PLANIFICACION}v1/servidorCivil/validaPlantillaSegmento?cicloId=${cicloId}`;


//    const url = `${Const.API_ENTIDAD}v1/servidorCivil/validaPlantillaSegmento?cicloId=${cicloId}`;

   // const url = `${Const.API_ENTIDAD}v1/servidorCivil/validaFormatoServCivil`;
    const tramaEnvio = {
      value: body.split('base64,')[1],
      entidadId,
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((res: ResponseRequest) => {
        if (res.status.success) {
          if (res.payload.servidorCivil.length === 0) {
            return true;
          } else {
            return res.payload;
          }
        } else {
          throw new Error(res.status.error.messages[0]).message;
        }
      })
    );
  } */


  uploadFileMasivo(archivo: any, cicloId: any): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const paramFD: FormData = new FormData();
    paramFD.append('archivo', archivo);
    paramFD.append('cicloId', cicloId);
    paramFD.append('entidadId', entidadId.toString());

    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/validaPlantillaSegmento`;

   const req = new HttpRequest(
    'POST',
    url,
    paramFD,
    {
      reportProgress: true,
      responseType: 'json',
    }
  );
  return this.http.request(req);
  }

  downloadExcel() { 
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/downloadFormatServCivil?idEntidad=${entidadId}&estadoSerCivGdrId=${Const.ESTADO_INACTIVO_SERVIDORES_CIVILES}`;
   // const url = `${Const.API_ENTIDAD}v1/servidorCivil/downloadFormatServCivil?idEntidad=${entidadId}`;

    return this.http.get(url).pipe(
        map(
            (response: ResponseRequest) => {
                if (!response.status.success) {
                    throw new Error(response.status.error.messages[0]).message;
                } else {
                    return response.payload;
                }
            }
        )
    );
  }

  getOrganigramas(): Observable<any[]> {
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }
    const url = `${Const.API_ENTIDAD}v1/organigrama/servidores/civiles?entidadId=${entidadId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaServidorCivil;
          }
        }
      )
    );
  }

  searchOrganigramaFilter(body: any): Observable<any[]> {
    let url;
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }

    const filterString = requestFilter(body);
    if (filterString.length > 1 ) {
      url = `${Const.API_ENTIDAD}v1/organigrama/servidores/civiles?entidadId=${entidadId}&${filterString}`;
    } else {
      url = `${Const.API_ENTIDAD}v1/organigrama/servidores/civiles?entidadId=${entidadId}`;
    }
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaServidorCivil;
      })
    );
  }

  getTiposDocumento(): Observable<MaestraParametro[]> {
    const url = `${Const.API_ENTIDAD}v1/maestra?tipoParametro=PER_TIPO_DOCUMENTO`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

  eliminarServidor(personaId: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/${personaId}?estado=0`;
    return this.http.delete(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  getDatosPersonales(personaId: number): Observable<DatosPersonalesServidorCivil> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/datosPersonales?entidadId=${entidadId}&personaId=${personaId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.datosPersonalesServidorCivil;
          }
        }
      )
    );
  }

  agregarServidor(datos: DatosPersonalesServidorCivil): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/crear`;
    let servidorCivil: any = datos;
    servidorCivil.apellidoPaterno = servidorCivil.apellidoPaterno.toUpperCase();
    servidorCivil.apellidoMaterno = servidorCivil.apellidoMaterno.toUpperCase();
    servidorCivil.nombres = servidorCivil.nombres.toUpperCase();
    servidorCivil.entidadId = this.authService.getCurrentUserValue.entidadId;
    servidorCivil.sindicatoId = datos.sindicato;
    servidorCivil.correoElectronico = datos.correoInstitucional;

    const body = { payload: { servidorCivil: servidorCivil } };
    return this.http.post(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  actualizarServidor(datos: DatosPersonalesServidorCivil): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/editar`;
    const body = {
      payload: {
          entidadId: entidadId,
          detuoId: datos.detalleuoId,
          personaId: datos.personaId,
          telefono: datos.telefono,
          correoInstitucional: datos.correoInstitucional,
          correoAlterno: datos.correoAlternativo,
          sindicatoFlag: datos.sindicato
      }
    };
    return this.http.put(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  agregarPuesto(datos: PuestoUoServidorCivil): Observable<any> {
    datos.entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/agregarPuesto`;
    const body = { payload: datos };

    return this.http.put(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  editarPuesto(datos: PuestoUoServidorCivil): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/actualizarDetallePuesto`;
    const payload = {
      ...datos,
      entidadId: this.authService.getCurrentUserValue.entidadId,
      nombrePuesto: datos.puesto
    };
    const body = { payload: payload };

    return this.http.put(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  getHisotrialPuestos(personaId: number, uoId: number): Observable<PuestoUoServidorCivil[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/puestoUO?entidadId=${entidadId}&personaId=${personaId}&uoId=${uoId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaPuestoUoServidorCivil;
          }
        }
      )
    );
  }
 
  listarPersonasParaPuesto(uoId: number, tipoAsignacion: string): Observable<any[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/personasPuesto?entidadId=${entidadId}&uoId=${uoId}&tipoAsignacion=${tipoAsignacion}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaPersonasPuestoUoServidorCivil;
          }
        }
      )
    );
  }

  listarParticipantes(body: any): Observable<IParticipanteEvaluador[]> {
    const filterString = requestFilter(body);
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }

    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
  
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/participantes?entidadId=${entidadId}&${filterString}&cicloId=${cicloId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }


  listarParticipantesServidoresCiviles(body: any): Observable<IParticipanteEvaluador[]> {
    const filterString = requestFilter(body);
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }

    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
  
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes?entidadId=${entidadId}&${filterString}&cicloId=${cicloId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

  listarEvaluadores(): Observable<IParticipanteEvaluador[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/evaluadores?entidadId=${entidadId}&cicloId=${cicloId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  listarEvaluadoresentidad(): Observable<IParticipanteEvaluador[]> {
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }

    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
  
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/entidad/evaluadores?entidadId=${entidadId}&cicloId=${cicloId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }
 
  listarEvaluadosEntidadSub(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    let entidadId = this.authService.getCurrentUserValue.entidadId;

    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }
 
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
  
   const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/entidad/evaluados?entidadId=${entidadId}&cicloId=${cicloId}` +
      `&uoId=${evaluador.uoId}&personaEvaluadorId=${evaluador.personaId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  listarEvaluados(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/participantes/evaluados?entidadId=${entidadId}` +
      `&uoId=${evaluador.uoId}&personaEvaluadorId=${evaluador.personaId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaParticipanteEvaluadosServidorCivil;
          }
        }
      )
    );
  }

  listarEvaluadosPersona(personaId: number): Observable<IParticipanteEvaluador[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url =
      `${Const.API_ENTIDAD}v1/servidorCivil/participantes/evaluadosPersona?entidadId=${entidadId}` +
      `&personaId=${personaId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaParticipanteEvaluadosServidorCivil;
          }
        }
      )
    );
  }

  updateSegmento(tramaEnvio: any): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/actualizarSegmento`;
    return this.http.put(url, tramaEnvio).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response;
          }
        }
      )
    );
  }

  activarParticipante(tramaEnvio: any): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/activarParticipante`;
    return this.http.put(url, tramaEnvio).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response;
          }
        }
      )
    );
  }

  listarActividades(cronogramaId, etapaId): Observable<any[]> {
    let url = "";
    if (etapaId) {
      url = `${Const.API_PLANIFICACION}v1/actividad/listar?idCronograma=${cronogramaId}&idEtapa=${etapaId}`;
    } else {
      url = `${Const.API_PLANIFICACION}v1/actividad/listar?idCronograma=${cronogramaId}`;
    }
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.items;
          }
        }
      )
    );
  }

  listarRolesPorSegmento(segmentoId: number, flagJefeUo: string): Observable<any[]> {
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/segmentoRol/${segmentoId}/jefeUo/${flagJefeUo}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

  listarSinEvaluador(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/evaluadosSinEvaluadores`;
    
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
    let request : {

    }
    if (evaluador.personaEvaluadorId == null) {
      request = {
          entidadId: entidadId + '',
          uoId: evaluador.uoId + '',
          cicloId: cicloId,
          detalleUOId: evaluador.detUnidadOrganicaId + '',
          personaId: evaluador.personaId + '',
          segmentoId: '3',
          esJefe: 'N'
      }
    } else {
      request = {
          entidadId: entidadId + '',
          uoId: evaluador.uoId + '',
          cicloId: cicloId,
          detalleUOId: evaluador.detUnidadOrganicaId + '',
          personaId: evaluador.personaId + '',
          segmentoId: '3',
          esJefe: 'N',
          personaEvaluadorId: evaluador.personaEvaluadorId + '', 
      }
    }
    return this.http
      .get(url, {
        params: request,
      })
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        })
      );
  }

  listarSinMandoMedio(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }

    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/evaluados/noMandoMedios`;
    return this.http
      .get(url, {
        params: {
          entidadId: entidadId + '',
          personaEvaluadorId: evaluador.personaId + '',
          uoId: evaluador.uoId + '',
          cicloId:  cicloId
        },
      })
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        })
      );
  }

  listarEvaluadosSinEvaluador(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
    let request : {

    }
    if (evaluador.personaEvaluadorId == null) {
      request = {
        entidadId: entidadId + '',
            uoId: evaluador.uoId + '',
            cicloId: cicloId,
            personaId: evaluador.personaId+ '',
            segmentoId: evaluador.segmentoId+'',
            esJefe: evaluador.esJefe,
            detalleUOId: evaluador.detUnidadOrganicaId + '',
      }
    } else {
      request = {
        entidadId: entidadId + '',
            uoId: evaluador.uoId + '',
            cicloId: cicloId,
            personaId: evaluador.personaId+ '',
            segmentoId: evaluador.segmentoId+'',
            esJefe: evaluador.esJefe,
            detalleUOId: evaluador.detUnidadOrganicaId + '',
            personaEvaluadorId: evaluador.personaEvaluadorId + '', 
      }
    }
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/evaluadosSinEvaluadores`;
    return this.http
      .get(url, {
        params : request,
      })
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        })
      );
  }

  eliminarEvaluado(evaluador: IParticipanteEvaluador): Observable<Status> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const { detUnidadOrganicaId, uoId } = evaluador;
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/quitar/evaluadorAsignadoUnEvaluado`;
    return this.http
      .delete(url, {
        params: {
          entidadId: entidadId + '',
          cicloId: cicloId,
          personaId: evaluador.personaId + '',
          detalleUOId: detUnidadOrganicaId + '',
        },
      })
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.status;
          }
        })
      );
  }

  agregarMandoMedio(mandoMedio: IParticipanteEvaluador, evaluados: IParticipanteEvaluador[]): Observable<Status> {
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/actualizar/nuevoEvaluadorMMEvaluados`;
    return this.http
      .put(url, {
        payload: {
          evaluadorMMdetUoId: mandoMedio.detUnidadOrganicaId,
          cicloId: cicloId,
          entidadId: entidadId,
          uoId: mandoMedio.uoId,
          personaEvaluadorId: mandoMedio.personaId,
          evaluados: evaluados.map(e => {
            return {
              detuoId: e.detUnidadOrganicaId,
              personaId: e.personaId,
              puestoId: e.puestoId,
              rolId: e.rolId
            };
          })
        }
      }) 
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.status;
          }
        })
      );
  }


  agregarMandoMedioEvaluado(mandoMedio: IParticipanteEvaluador, evaluados: IParticipanteEvaluador[]): Observable<Status> {
    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/actualizar/Evaluador`;
    return this.http
      .put(url, {
        payload: {
          evaluadorMMdetUoId: mandoMedio.detUnidadOrganicaId,
          entidadId: entidadId,
          cicloId: cicloId,
          uoId: mandoMedio.uoId,
          personaEvaluadorId: mandoMedio.personaId,
          evaluados: evaluados.map(e => {
            return {
              detuoId: e.detUnidadOrganicaId,
              personaId: e.personaId,
              puestoId: e.puestoId,
              rolId: e.rolId 
            };
          })
        }
      }) 
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.status;
          }
        })
      );
  }

  listarMandosMedio(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }

    let ciclo = JSON.parse(sessionStorage.getItem('ciclo'));

    let cicloId ;
    if(ciclo) {
      cicloId = ciclo.cicloId;
    }

    const url = `${Const.API_PLANIFICACION}v1/servidorCivil/participantes/mandoMedio`;
    return this.http
      .get(url, {
        params: {
          entidadId: entidadId + '',
          personaEvaluadorId: evaluador.personaId + '',
          uoId: evaluador.uoId + '',
          cicloId: cicloId 

        },
      })
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        })
      );
  }







/*
  subirFormatoMetas(body: any, archivo: any): Observable<any> {
    const paramFD: FormData = new FormData();
    paramFD.append('archivo', archivo);
    paramFD.append('cicloId', body.cicloId);
    paramFD.append('entidadId', body.entidadId);
    paramFD.append('personaId', body.personaId);
    paramFD.append('detaUoId', body.detaUoId);
    //const url = `${Const.API_PLANIFICACION}v1/servidorcivil/subirFormatoMetas`;
    const url = `http://localhost:8080/planificacion/api/public/v1/servidorcivil/subirFormatoMetas`;
    const req = new HttpRequest(
      'POST',
      url,
      paramFD,
      {
        reportProgress: true,
        responseType: 'json',
      }
    );
    return this.http.request(req);
  }
*/


  listarMetasParticipante(personaId: number, detUoId: number, cicloId: number): Observable<PayloadMetas> {
    let url = `${Const.API_PLANIFICACION}v1/meta/participante/${personaId}/detalleUo/${detUoId}/ciclo/${cicloId}`;
    console.log(url);

 //   meta/participante/{​personaId}​/detalleUo/{​detUoId}​/ciclo/{​cicloId}​
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }

/*   listarMetasParticipante(personaId: number, detUoId: number, cicloId: number): Observable<PayloadMetas> {
    let url = `http://10.240.132.34:8088/planificacion/api/public/v1/meta/participante/580/detalleUo/535/ciclo/4`;
   // meta/participante/{personaId}/detalleUo/{detUoId}/ciclo/{cicloId}
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          console.log(response);
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }
 */

  validarMeta(body: any): Observable<any> {
    // const url = `http://10.240.132.34:8088/planificacion/api/public/v1/meta/validacion`;
    let url = `${Const.API_PLANIFICACION}v1/meta/validacion`;
    console.log(url);
    return this.http.put(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          console.log(response);
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]);
          } else {
            return response;
          }
        }
      )
    );
  }

  observarMeta(body: any): Observable<any> {
    // const url = `http://10.240.132.34:8088/planificacion/api/private/v1/meta/editar/observaciones`;
    let url = `${Const.API_PLANIFICACION}v1/meta/editar/observaciones`;

    console.log(url);
    return this.http.put(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          console.log(response);
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]);
          } else {
            return response;
          }
        }
      )
    );
  }

  listarEvaluadosSinEvaluadorEntidad(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/participantes/entidad/evaluadosSinEvaluadores`;
    return this.http
      .get(url, {
        params: {
          entidadId: entidadId + ''
        },
      })
      .pipe(
        map((response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.listaParticipanteEvaluadosSinEvaluador;
          }
        })
      );
  }


/*   validarMeta(body: any): Observable<any> {
    const url = `http://10.240.132.34:8088/planificacion/api/public/v1/meta/validacion`;
    return this.http.post(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          console.log(response);
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  } */

/*
  agregarMetas(body: any): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/meta`;
    return this.http.post(url, body).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload;
          }
        }
      )
    );
  }
 */


}
