import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { Organo } from '../model/organo';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { ToastService } from 'src/app/@presentation/@common-components/toast';


@Injectable({
    providedIn: 'root'
})

export class OrganoService implements OrganoRepository {

    unidadOrganica: Organo[] = [];
    organos: Organo[] = [];


    organoToEditFromChart: Organo = null;

    constructor(
        private http: HttpClient,
        private authService: AuthenticationRepository,
    private toastService: ToastService

    ) { }

    setOrganoFromChart(organo: Organo) {
        this.organoToEditFromChart = organo;
    }

    getOrganos(toUpdateValues: boolean): Observable<Organo[]> {
        const entidadId = this.authService.getCurrentUserValue.entidadId;
      const url = `${Const.API_ENTIDAD}v1/organigrama/filter?entidadId=${entidadId}`;
     // if (this.organos) {
        const request = this.http.get(url).pipe(
            map(
                (response: any) => {
                    this.organos = response.payload.listaOrganigrama;
                    return response.payload.listaOrganigrama;
            
                }
            )
        );
        return toUpdateValues ?
            request : this.organos.length > 0 ?
                of(this.organos) : request;
  /*     } else {
        this.toastService.showToast(
            'Aún no hay data registrada',
            'danger'
          );
      } */
       
    }


    /* getOrganos(toUpdateValues: boolean): Observable<Organo[]> {
        const entidadId = this.authService.getCurrentUserValue.entidadId;
      const url = `${Const.API_ENTIDAD}v1/organigrama/list?entidadId=81`;
        const request = this.http.get(url).pipe(
            map(
                (response: any) => {
                   console.log('%corgano.service.ts line:35 response', 'color: #007acc', response);
                    this.unidadOrganica = response.payload.listaOrganigrama.listaOrganigramaHijo[0];
                    console.log('%corgano.service.ts line:59 this.unidadOrganica ', 'color: yellow;', this.unidadOrganica );

                    return response.payload.listaOrganigrama.listaOrganigramaHijo[0];
                }
            )
        );
        return toUpdateValues ?
            request : this.unidadOrganica.length > 0 ?
                of(this.unidadOrganica) : request;
    } */


    searchOrganos(query: string): Observable<Organo[]> {
        const entidadId = this.authService.getCurrentUserValue.entidadId;
        const url = `${Const.API_ENTIDAD}v1/organigrama/filter?texto=${query}&entidadId=${entidadId}&tipo=81`;
        return this.http.get(url).pipe(
            map(
                (response: any) => {
                    return response.payload.listaOrganigrama;
                }
            )
        );
    }

    deleteOrgano(organigramaId: number): Observable<any> {
        const url = `${Const.API_ENTIDAD}v1/organigrama/${organigramaId}?estado=0`;
        return this.http.delete(url).pipe(
            map(
                (response: ResponseRequest) => {
                    if (!response.status.success) {
                        throw new Error(response.status.error.messages[0]).message;
                    } else {
                        return true;
                    }
                }
            )
        );
    }

    createOrUpdateOrgano(body: any, flag?: boolean, idOrgano?: number, organo?: Organo): Observable<any> {
        const tramaEnvio = {
            trace: {
                traceId: "string"
            },
            payload: {
                estadoRegistro: body.estado,
                entidadId: this.authService.getCurrentUserValue.entidadId,
                areaId: null,
                sedeId: null,
                padreOrganigramaId: body.organoQueDepende,
                puesto: body.puesto,
                orden: 0,
                nivel: body.nivel,
                tipoOrganoUoId: 81, // fijo
                naturalezaOrgano: body.naturaleza,
                nivelGobiernoId: null,
                descripcion: body.nombreOrgano,
                descripcionCorta: null,
                sigla: body.sigla.toUpperCase(),
                tipoDocumento: body.tipoDocumento,
                numeroDocumento: body.numeroDocumento,
                nombres: body.nombres.toUpperCase(),
                apellidoPaterno: body.apellidoPaterno.toUpperCase(),
                apellidoMaterno: body.apellidoMaterno?.toUpperCase(),
                telefono: body.celular,
                telefonoId: organo?.telefonoId || null,
                correo: body.correoLaboral.toLowerCase(),
                correoId: organo?.correoId || null,
                paisId: body.paisObject?.paisId || null
            }
        };

        if (flag) {
            const url = `${Const.API_ENTIDAD}v1/organigrama/${idOrgano}`;
            return this.http.put(url, tramaEnvio).pipe(
                map(
                    (response: ResponseRequest) => {
                        if (!response.status.success) {
                            throw new Error(response.status.error.messages[0]).message;
                        } else {
                            return true;
                        }
                    }
                )
            );
        } else {
            const url = `${Const.API_ENTIDAD}v1/organigrama`;
            return this.http.post(url, tramaEnvio).pipe(
                map(
                    (response: ResponseRequest) => {
                        if (!response.status.success) {
                            throw new Error(response.status.error.messages[0]).message;
                        } else {
                            return true;
                        }
                    }
                )
            );
        }

    }

    downloadExcel() {
        const entidadId = this.authService.getCurrentUserValue.entidadId;
        const url = `${Const.API_ENTIDAD}v1/organigrama/downloadFormat?idEntidad=${entidadId}`;
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


}
