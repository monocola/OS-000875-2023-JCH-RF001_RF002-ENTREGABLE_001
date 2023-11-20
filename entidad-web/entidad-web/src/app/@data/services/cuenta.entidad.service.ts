import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { from, Observable } from 'rxjs';
import { catchError, map, mergeMap, timeout, toArray } from 'rxjs/operators';
import { Const } from './const';
import { CuentaEntidadRepository } from '../../@domain/repository/cuenta.entidad.repository';
import {
  CuentaEntidad,
  CuentaEntidadRequest,
  CuetaEntidadResponse,
} from '../model/cuentaentidad';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';
import { Utils } from '../../utils/utils';
@Injectable({
  providedIn: 'root',
})
export class CuentaEntidadService extends CuentaEntidadRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  getCuentasEntidad(): Observable<CuentaEntidad[]> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/filter?idEntidad=${this.authenticationRepository.getCurrentUserValue.entidadId}`;
    return this.http
      .get<CuetaEntidadResponse>(url)
      .pipe(
        map((response) => {
          return response.payload.listaEntidades;
        })
      )
      .pipe(mergeMap((value) => from(value)))
      .pipe(
        map((value) => {
          value.apellidos = `${
            value.apellidoPaterno + ' ' + value.apellidoMaterno
          }`;
          value.nombreCompleto = `${value.nombres + ' ' + value.apellidos}`;
          value.editable = +value.flagCuentaEditable === 1;
          value.listaRoles = value.listaRoles.map((itemRol) => {
            itemRol.editable = value.editable;
            return itemRol;
          });
          return value;
        })
      )
      .pipe(toArray());
  }

  saveRegistro(
    cuentaEntidadRequest: CuentaEntidadRequest
  ): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/`;
    let roles = cuentaEntidadRequest.rol.map((value) => {
      return { usuarioId: null, rolId: value };
    });
    let body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        cuentaEntidad: {
          entidadId: this.authenticationRepository.getCurrentUserValue
            .entidadId,
          personaId: cuentaEntidadRequest.personaId,
          areaId: null,
          puestoTrabajoId: cuentaEntidadRequest.puesto,
          correoId: null,
          telefonoId: null,
        },
        personaNatural: {
          tipoPersona: Const.PER_TIPO_PERSONA_NAT,
          tipoDocumento: cuentaEntidadRequest.typeDocument,
          numeroDocumento: cuentaEntidadRequest.numberDocument,
          nombres: cuentaEntidadRequest.name,
          apellidoPaterno: cuentaEntidadRequest.fatherName,
          apellidoMaterno: cuentaEntidadRequest.motherName,
          apellidoCasada: null,
          fechaNacimiento: null,
          sexo: null,
          estadoCivil: null,
          paisId: cuentaEntidadRequest.country,
          direccionCompleta: null,
          referenciaDireccion: null,
          correoPrincipal: cuentaEntidadRequest.email,
          correoSecundario: null,
          correoLaboral: null,
          telefonoFijo: cuentaEntidadRequest.phone,
          anexo: cuentaEntidadRequest.anexo,
          celularPrincipal: null,
          celularSecundario: null,
          celularLaboral: null,
          rutaPaginaWeb: null,
        },
        listaRoles: roles,
      },
    };

    return this.http.post(url, body).pipe(
      map((response: any) => response.result === 200 ? response : response),
      timeout(10000),
      catchError((e) => {
        if (e.message) {
          throw new Error(e.message).message;
        } else {
          throw new Error('El servicio no está disponible, intente más tarde')
            .message;
        }
      })
    );
  }

  updateCuenta(
    cuentaEntidadRequest: CuentaEntidadRequest
  ): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/${cuentaEntidadRequest.cuentaId}`;
    let roles = cuentaEntidadRequest.rol;
    let body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        cuentaEntidad: {
          entidadId: this.authenticationRepository.getCurrentUserValue
            .entidadId,
          personaId: cuentaEntidadRequest.personaId,
          areaId: null,
          puestoTrabajoId: cuentaEntidadRequest.puesto,
          correoId: null,
          telefonoId: null,
        },
        telefono: {
          telefonoId: cuentaEntidadRequest.telefonoId,
          personaId: cuentaEntidadRequest.personaId,
          tipoTelefono: null,
          codigoArea: null,
          numeroTelefono: cuentaEntidadRequest.phone,
          numeroAnexo: cuentaEntidadRequest.anexo,
        },
        correo: {
          correoId: cuentaEntidadRequest.correoId,
          personaId: cuentaEntidadRequest.personaId,
          tipoCorreo: 'PRINC', // null
          correo: cuentaEntidadRequest.email,
        },
        listaRoles: roles,
      },
    };

    return this.http.put(url, body).pipe(
      map((response: any) => (response.result === 200 ? response : response)),
      timeout(5000),
      catchError((e) => {
        if (e.message) {
          throw new Error(e.message).message;
        } else {
          throw new Error('El servicio no está disponible, intente más tarde')
            .message;
        }
      })
    );
  }
  deleteCuenta(cuentaId: number, estado: number): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/${cuentaId}?estado=${estado}`;
    return this.http.delete(url).pipe(
      map((response: any) => response.result === 200),
      timeout(5000),
      catchError((e) => {
        if (e.message) {
          throw new Error(e.message).message;
        } else {
          throw new Error('El servicio no está disponible, intente más tarde')
            .message;
        }
      })
    );
  }
  validCuenta(cuentaId: number): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/${cuentaId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return +response.payload.tareaCuentaEntidad[0].flagTareaPendiente === 1;
      }),
      timeout(5000),
      catchError((e) => {
        if (e.message) {
          throw new Error(e.message).message;
        } else {
          throw new Error('El servicio no está disponible, intente más tarde')
            .message;
        }
      })
    );
  }
  updateUsuarioRol(
    usuarioRolId: number,
    active: boolean,
    fechaAlta?: string
  ): Observable<boolean> {
    const url = `${Const.API_SEGURIDAD}v1/usuarioRoles/${usuarioRolId}`;
    let body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        usuarioRolId: usuarioRolId,
        fechaInicioVigencia: active ? Utils.getfechaActual() : fechaAlta,
        fechaFinVigencia: active
          ? Utils.getfechaActualMas1Year()
          : Utils.getfechaActual(),
        estado: active ? '1' : '0',
      },
    };
    return this.http.put(url, body).pipe(
      map((response: any) => {
        return response.status.success;
      }),
      timeout(5000),
      catchError((e) => {
        if (e.message) {
          throw new Error(e.message).message;
        } else {
          throw new Error('El servicio no está disponible, intente más tarde')
            .message;
        }
      })
    );
  }

  reasignarCtaAsociada(
    cuentaEntidadRequest: CuentaEntidadRequest,
    cuentaId: number
  ): Observable<boolean> {
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/reasignar/${cuentaId}`;
    let roles = cuentaEntidadRequest.rol.map((value) => {
      return { usuarioId: null, rolId: value };
    });
    let body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        cuentaEntidad: {
          entidadId: this.authenticationRepository.getCurrentUserValue
            .entidadId,
          personaId: cuentaEntidadRequest.personaId,
          areaId: null,
          puestoTrabajoId: cuentaEntidadRequest.puesto,
          correoId: null,
          telefonoId: null,
          motivo: cuentaEntidadRequest.commet,
        },
        personaNatural: {
          tipoPersona: Const.PER_TIPO_PERSONA_NAT,
          tipoDocumento: cuentaEntidadRequest.typeDocument,
          numeroDocumento: cuentaEntidadRequest.numberDocument,
          nombres: cuentaEntidadRequest.name,
          apellidoPaterno: cuentaEntidadRequest.fatherName,
          apellidoMaterno: cuentaEntidadRequest.motherName,
          apellidoCasada: null,
          fechaNacimiento: null,
          sexo: null,
          estadoCivil: null,
          paisId: cuentaEntidadRequest.country,
          direccionCompleta: null,
          referenciaDireccion: null,
          correoPrincipal: cuentaEntidadRequest.email,
          correoSecundario: null,
          correoLaboral: null,
          telefonoFijo: cuentaEntidadRequest.phone,
          anexo: cuentaEntidadRequest.anexo,
          celularPrincipal: null,
          celularSecundario: null,
          celularLaboral: null,
          rutaPaginaWeb: null,
        },
        listaRoles: roles,
      },
    };

    return this.http.put(url, body).pipe(
      map((response: any) => response.status.success),
      timeout(10000),
      catchError((e) => {
        if (e.message) {
          throw new Error(e.message).message;
        } else {
          throw new Error('El servicio no está disponible, intente más tarde')
            .message;
        }
      })
    );
  }

  getRolesForModal() {
    const url = `${Const.API_SEGURIDAD}v1/aplicaciones/${Const.APPLICATION_ID}/roles/menus`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const menuItems = response.payload.items;
        const roles = Object.keys(response.payload.items[0]);
        roles.splice(0, 4);
        const funciones = menuItems.filter(
          (menu) => menu.MENU_ID_PADRE == null && menu.MENU_ID !== 185
        );
        const objectFunciones = [];
        funciones.map((funcion) => {
          objectFunciones.push({
            funcion: funcion,
            hijos: menuItems.filter(
              (menuItem) => menuItem.MENU_ID_PADRE === funcion.MENU_ID
            ),
          });
        });
        const tramaRetorno = {
          roles: roles.filter((r) => r !== 'Administrador Servir'),
          body: objectFunciones,
        };
        return tramaRetorno;
      })
    );
  }

  getAllRoles(): Observable<any> {
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/aplicaciones/${aplicacionId}/roles`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        return response.payload.items.filter(
          (item) => item.rolId !== Const.R_ADMIN_SERVIR
        );
      })
    );
  }

  getPersonasPorRoles(rolId: number): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_ENTIDAD}v1/cuentaentidad/persona?idEntidad=${entidadId}&idRol=${rolId}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        return response.payload.items;
      })
    );
  }
}
