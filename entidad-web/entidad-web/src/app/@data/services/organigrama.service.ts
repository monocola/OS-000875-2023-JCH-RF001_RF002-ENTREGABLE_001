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

@Injectable({
  providedIn: 'root',
})
export class OrganigramaService implements OrganigramaRepository {
  organos: Organo[] = [];
  organoToEditFromChart: any = null;

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
    const url = `${Const.API_ENTIDAD}v1/organigrama/masivo`;
    const tramaEnvio = {
      value: body.split('base64,')[1],
      entidadId,
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((res: ResponseRequest) => {
        if (res.status.success) {
          if (res.payload.organo.length === 0) {
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
}

export interface Persona {
  nombres: string;
  apellidoPaterno: string;
  apellidoMaterno: string;
  personaResponsableId: number;
  nombreCompleto?: string;
}
