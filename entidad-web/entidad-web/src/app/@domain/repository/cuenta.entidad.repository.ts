import { Observable } from 'rxjs';
import {
  CuentaEntidad,
  CuentaEntidadRequest,
} from '../../@data/model/cuentaentidad';

export abstract class CuentaEntidadRepository {
  abstract getCuentasEntidad(): Observable<CuentaEntidad[]>;
  abstract saveRegistro(
    cuentaEntidadRequest: CuentaEntidadRequest
  ): Observable<boolean>;
  abstract updateCuenta(
    cuentaEntidadRequest: CuentaEntidadRequest
  ): Observable<boolean>;
  abstract deleteCuenta(cuentaId: number, estado: number): Observable<boolean>;
  abstract validCuenta(cuentaId: number): Observable<boolean>;
  abstract updateUsuarioRol(
    usuarioRolId: number,
    active: boolean,
    fechaAlta?: string
  ): Observable<boolean>;
  abstract reasignarCtaAsociada(
    cuentaEntidadRequest: CuentaEntidadRequest,
    cuentaId: number
  ): Observable<boolean>;
  abstract getRolesForModal(): Observable<any>;
  abstract getAllRoles(): Observable<any>;
  abstract getPersonasPorRoles(rolId: number): Observable<any>;
}
