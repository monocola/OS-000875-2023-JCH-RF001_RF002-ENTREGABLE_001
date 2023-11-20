import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { requestFilter } from 'src/app/utils/general';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Organo } from '../model/organo';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { Utils } from 'src/app/utils/utils';

@Injectable({
  providedIn: 'root',
})
export class OrganigramaService implements OrganigramaRepository {
  organos: Organo[] = [];
  organoToEditFromChart: any = null;
  organigrama: any[] = [];

  constructor(
    private http: HttpClient,
    private authService: AuthenticationRepository
  ) {}
  getPeopleAdmin(value: string): Observable<any[]> {
    const namePerson = value.toUpperCase();
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/organigrama/combo?entidadId=${entidadId}&nombreApellidos=${namePerson}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const personas: Persona[] = response.payload.comboPersonas;
        personas.forEach(
          (p) =>
            (p.nombreCompleto = `${p.nombres} ${p.apellidoPaterno} ${
              p.apellidoMaterno || ''
            }`.trim())
        );
        return response.payload.comboPersonas;
      })
    );
  }

  setOrganoOrUnidadFromChart(organo: any) {
    let dataToSet = null;
    if (organo) {
      dataToSet = {
        apellidoMaterno: organo.apellidoMaterno || null,
        apellidoPaterno: organo.apellidoPaterno,
        areaId: organo.areaId,
        correo: organo.correo,
        correoId: organo.correoId,
        desNaturaleza: organo.desNaturaleza,
        desNivel: organo.desNivel,
        descripOrganoPadre: '',
        descripcion: organo.descripcion,
        descripcionCorta: organo.descripcionCorta,
        desTipoOrgano: organo.desTipoOrgano || organo.descripcionTipoOrg,
        entidadId: organo.idEntidad || organo.entidadId,
        estado: organo.estado,
        estadoRegistro: organo.estadoId || organo.estadoRegistro,
        naturalezaOrgano: organo.naturalezaId || organo.naturalezaOrgano,
        nivel: organo.nivelId || organo.nivel,
        nivelGobiernoId: null,
        nombrePais: organo.nombrePais,
        nombres: organo.nombres,
        nroDocumento: organo.numeroDocumento || organo.nroDocumento,
        orden: organo.orden,
        organigramaId: organo.idOrganigrama || organo.organigramaId,
        padreOrganigramaId: organo.padreId || organo.padreOrganigramaId,
        paisId: organo.paisId,
        personaResponsableId: organo.personaResponsableId,
        puesto: organo.puesto,
        sedeId: organo.sedeId,
        sigla: organo.sigla,
        telefono: organo.telefono,
        telefonoId: organo.telefonoId,
        tipoDocumento: organo.tipoDocumentoId || organo.tipoDocumento,
        tipoOrganoUoId: organo.tipoOrganoId || organo.tipoOrganoUoId,
        unidadOrganica: organo.descripcion || organo.unidadOrganica,
      };
      if (dataToSet.padreOrganigramaId === '00000000') {
        dataToSet.padreOrganigramaId = '';
      }
    }
    this.organoToEditFromChart = dataToSet;
  }

  getOrganoStored() {
    return this.organoToEditFromChart;
  }

  searchOrganigramas(body: any): Observable<any[]> {
    body.responsable = body.responsable?.personaResponsableId || '';
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const filterString = requestFilter(body);
    const url = `${Const.API_ENTIDAD}v1/organigrama?entidadId=${entidadId}&${filterString}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaOrganigrama;
      })
    );
  }

  uploadFileMasivo(body: any): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/organigrama/validaFormatoOrganigrama`;
    const tramaEnvio = {
      value: body.split('base64,')[1],
      entidadId,
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((res: ResponseRequest) => {
        if (res.status.success) {
          if (res.payload.organigrama.length === 0) {
            return true;
          } else {
            return res.payload;
          }
        } else {
          throw new Error(res.status.error.messages[0]).message;
        }
      })
    );
  }


  downloadExcel() {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/organigrama/downloadFormatOrganigrama?idEntidad=${entidadId}`;
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

  getGestionOrganigramaByFiltro(body: any) {
      let url;
      let entidadId = this.authService.getCurrentUserValue.entidadId;
      let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
      if(monitoreo) {
        entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
      }
      const filterString = requestFilter(body);
      if (filterString.length > 1 ) {
        url = `${Const.API_ENTIDAD}v1/organigrama/gestion/listar?entidadId=${entidadId}&${filterString}`;
      } else {
        url = `${Const.API_ENTIDAD}v1/organigrama/gestion/listar?entidadId=${entidadId}`;
      }
      return this.http.get(url).pipe(
        map((response: any) => {
          return response.payload.listaGestionOrganigrama;
        })
      );


  }

  registerOrUpdateUO(body, organigramaId?): Observable<boolean> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        organigramaId: organigramaId ? organigramaId : null,

        entidadId: entidadId,
        estadoRegistro: body.estadoRegistro,
        padreOrganigramaId: body.unidadOrganicaSuperiorId,
        tipoOrganoUoId: body.tipoOrganoUoId,
        orden: 0, // body.orden,
        naturalezaOrgano: body.tipoOrganoId,
        descripcion: body.descripcion,
        sigla: body.sigla,

      },
    };

    let url = '';

    if (organigramaId) {
      url = `${Const.API_ENTIDAD}v1/organigrama/registrar/${organigramaId}`;
      return this.http.put(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.payload.organigrama) {
            return true;
          } else {
            return false;
          }
        })
      );
    } else {
      url = `${Const.API_ENTIDAD}v1/organigrama/registrar`;
      return this.http.post(url, tramaEnvio).pipe(
        map((response: ResponseRequest) => {
          if (response.payload.organigrama) {
            return true;
          } else {
            return false;
          }
        })
      );
    }
  }


  deleteGO(organigramaId: any): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/organigrama/eliminar/${organigramaId}?estado=0`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          return false;
        }
      })
    );
  }


  validarSiglaUnicidad(mensaje: any): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/organigrama/validaDuplicidad?entidadId=61&sigla=${mensaje}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.validaOrganigrama;
      })
    );
  }


  validarDescripcionUnicidad(mensaje: any): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/organigrama/validaDuplicidad?entidadId=61&descripcion=${mensaje}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.validaOrganigrama;
      })
    );
  }

  getListPuesto(entidadId: number, personaId: number): Observable<any[]> {
    let _entidadId = entidadId;
    if (!entidadId) {
      _entidadId = this.authService.getCurrentUserValue.entidadId;
    }
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/puestoUO?entidadId=${_entidadId}&personaId=${personaId}`;
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

  getDatosPersonales(entidadId: number, personaId: number): Observable<any[]> {
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

  getDatosPersona(entidadId: number, personaId: number): Observable<any[]> {
    const url = `${Const.API_ENTIDAD}v1/perfilUsuario?entidadId=${entidadId}&personaId=${personaId}`;
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

  updateDatosPersonales(body: any): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/perfilUsuario/editar`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidadId: body.entidadId,
        personaId: body.personaId,
        telefono: body.telefono,
        correoAlterno: body.correoAlterno,
        sindicatoFlag: body.sindicatoFlag,
        foto: {
          flag: body.foto.flag,
          fileBase64: body.foto.fileBase64,
          fileName: body.foto.fileName
        }
      },
    };
    return this.http.put(url, tramaEnvio).pipe(
      map(
        (response: ResponseRequest) => {
          // if (!response.status.success) {
            // throw new Error(response.status.error.messages[0]).message;
          // } else {
            return response;
          // }
        }
      )
    );
  }

}

export interface Persona {
  nombres: string;
  apellidoPaterno: string;
  apellidoMaterno: string;
  personaResponsableId: number;
  nombreCompleto?: string;
}
