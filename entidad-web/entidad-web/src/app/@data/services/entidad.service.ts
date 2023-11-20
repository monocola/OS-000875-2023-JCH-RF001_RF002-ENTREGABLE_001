import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { EntidadRepository } from '../../@domain/repository/entidad.repository';
import { Entidad, EntidadAdapter } from '../model/entidad';
import { getMiliseconds } from 'src/app/utils/general';

@Injectable({
  providedIn: 'root',
})
export class EntidadService extends EntidadRepository {
  entidad: Entidad = null;

  constructor(
    private http: HttpClient,
    private entidadAdapter: EntidadAdapter
  ) {
    super();
  }

  getRegistro(entidadId): Observable<Entidad> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${entidadId}`;
    return this.http.get<Entidad>(url).pipe(
      map((response: any) => {
        if (response.status.success) {
          this.entidad = this.entidadAdapter.adapt(response.payload.entidad[0]);
          return this.entidadAdapter.adapt(response.payload.entidad[0]);
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveRegistro(
    entidadForm: Entidad,
    imgData64: string,
    imgFile: File,
    entidad: Entidad,
    flagUpdatePhoto: number,
    imgData64P: string,
    imgFileP: File,
    flagUpdatePhotoP: number
  ): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${entidad.entidadId}`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        entidad: {
          personaId: entidad.personaId,
          adminTalentoId: null,
          fechaAlta: null,
          ubigeoId: null,
          nivelGobiernoId: null,
          sectorId: null,
          tipoEntidadId: null,
          comparteRuc: null,
          descripcionEntidad: entidadForm.descripcionEntidad,
          urLogoEntidad: null,
          urlWebEntidad: entidadForm.urlWeb,
          direccionId: null,
          sigla: entidadForm.sigla,
          direccion: entidadForm.direccion,
          urlPortada: null,
        },
        telefonos: [
          {
            telefonoId: null,
            personaId: entidad.personaId,
            tipoTelefono: null,
            codigoArea: null,
            numeroTelefono: entidadForm.telefono,
            numeroAnexo: entidadForm.anexo,
          },
        ],
        correos: [
          {
            correoId: null,
            personaId: entidad.personaId,
            tipoCorreo: null,
            correo: entidadForm.correo,
          },
        ],

        logo: {
          flag: flagUpdatePhoto,
          fileBase64: imgData64 ? imgData64.split('base64,')[1] : null,
          fileName: imgFile
            ? getMiliseconds() +
              '.' +
              imgFile.name.split('.')[imgFile.name.split('.').length - 1]
            : null,
        },

        portada: {
          flag: flagUpdatePhotoP,
          fileBase64: imgData64P ? imgData64P.split('base64,')[1] : null,
          fileName: imgFileP
            ? getMiliseconds() +
              '.' +
              imgFileP.name.split('.')[imgFileP.name.split('.').length - 1]
            : null,
        },
      },
    };

    return this.http.put(url, tramaEnvio).pipe(
      map((response: any) => {
        if (response.status.success) {
          return true;
        }
      })
    );
  }

  getEntidades () {
    const url = `${Const.API_ENTIDAD}v1/entidad/`;
    return this.http.get<Entidad>(url).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response.payload.entidad;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getUsuariosEntidad(entidadId: string) {
    const url = `${Const.API_SEGURIDAD}v1/entidades/${entidadId}/usuarios`;
    return this.http.get (url).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
}
