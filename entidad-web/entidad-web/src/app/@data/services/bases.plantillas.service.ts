import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BasesPlantillasRepository } from 'src/app/@domain/repository/bases-plantillas.repository';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { requestFilter } from 'src/app/utils/general';
import { ResponseRequest } from '../model/reponse-request';

@Injectable({
  providedIn: 'root',
})
export class BasesPlantillasService implements BasesPlantillasRepository {
  entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;

  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {}

  // Servicios - Repositorio

  getPlantillasBase(body: any): Observable<any> {
    const params = requestFilter({
      tipoInfo: body?.tipoInforme,
      titulo: body?.nombreInforme,
      estado: body?.estado,
      idInforme: body?.idInforme,
    });

    const url = `${Const.API_CONVOCATORIA}v1/informedetalle/${this.entidadId}?${params}`;
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

  getBonificacionList(informeId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/informedetalle/bonificacion?idInformeDetalle=${informeId}`;
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

  saveOrUpdatePlantillaBase(body: any, informeDetalleId: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        informeDetalleId: informeDetalleId || null,
        tipoInfo: body.tipoInformeId || null,
        nombretipoInfo: body.tipoInforme || null,
        titulo: body.nombreInforme || null,
        contenido: body.contenidoInforme || null,
        entidadId: this.entidadId || null,
        estado: 1,
        bonificacionList: this.setBonificaciones(body.bonificaciones),
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/informedetalle`;
    let request = informeDetalleId
      ? this.http.put(url, tramaEnvio)
      : this.http.post(url, tramaEnvio);
    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  deleteBonificacionList(informeId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/informedetalle/cambioestado?idInformeDetalle=${informeId}&estado=0`;
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

  // Funciones

  setBonificaciones(bonificaciones: any[]) {
    return (
      bonificaciones?.map((b) => {
        return {
          bonificacionId: b.id || null,
          tipoBonificacion: b.form.tipoBonificacion.maeDetalleId,
          bonificacionNombre: b.form.tipoBonificacion.descripcion,
          titulo: b.form.titulo,
          contenido: b.form.contenido,
          estado: b.estado,
          informeDetalleId: b.informeDetalleId || null,
          bonificacionDetalleDTOList: b.form.bonificaciones
            .map((bon) => {
              return {
                bonificacionDetalleId: bon.id,
                bonificacionId: b.id,
                descripcion: bon.descripcion,
                nivelId: bon.niveles.maeDetalleId,
                aplicaId: bon.aplicaSobre.maeDetalleId,
                porcentajeBono: bon.porcentaje,
                estado: 1,
              };
            })
            .concat(
              b.form.bonificacionesToDelete.map((bonToDelete) => {
                return {
                  bonificacionDetalleId: bonToDelete.id,
                  bonificacionId: b.id,
                  descripcion: bonToDelete.descripcion,
                  nivelId: bonToDelete.niveles.maeDetalleId,
                  aplicaId: bonToDelete.aplicaSobre.maeDetalleId,
                  porcentajeBono: bonToDelete.porcentaje,
                  estado: 0,
                };
              })
            ),
        };
      }) || []
    );
  }
}
