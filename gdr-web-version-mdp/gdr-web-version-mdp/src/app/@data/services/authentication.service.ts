import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpRequest } from '@angular/common/http';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { catchError, concatMap, map, timeout } from 'rxjs/operators';
import { Const } from './const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

import { JwtHelperService } from '@auth0/angular-jwt';
import { Router } from '@angular/router';
import { SidenavService } from './sidenav.service';
import { User } from '../model/user';
import { UserRoles } from '../model/userRoles';
import { ResponseRequest } from '../model/reponse-request';
import { ToastService } from '../../@presentation/@common-components/toast';

@Injectable({ providedIn: 'root' })
export class AuthenticationService extends AuthenticationRepository {
  private currentUserSubject: BehaviorSubject<User>;
  public currentUser: Observable<User>;
  private applicationRoles = [];
  private userRoles = [];
  private valToken: string = null;
  public entidadesIds: string;
  public rolId: number;
  nombreRol = '';

  constructor(
    private http: HttpClient,
    private jwtHelper: JwtHelperService,
    private router: Router,
    private sidenavService: SidenavService,
    private toast: ToastService
  ) {
    super();
    // Falta el endpoint de obtener data de usuario por mientras sera del api persona
    this.currentUserSubject = new BehaviorSubject<User>(
      JSON.parse(sessionStorage.getItem('persona'))
    );
    console.log(" personaaaaaaaaaaaaaaaaaa",this.currentUserSubject.value)
    this.currentUser = this.currentUserSubject.asObservable();
  }

  public get getCurrentUserValue(): User {
    return this.currentUserSubject.value;
  }

  login(login: string, clave: string): Observable<any> {
    this.currentUserSubject.next(null);
    return this.verifyCredentials(login, clave).pipe(
      concatMap((res) => this.getUserRoles()),
      concatMap((res) => this.getUserRolesPerfil()),
      concatMap((res) => this.getRoleIdentity()),
      concatMap((res) => this.getOptionsMenu()),
      concatMap((res) => this.getDataReniec()),
      catchError((e) => {
        throw e;
      })
    );
  }

  verifyCredentials(login: string, clave: string): Observable<any> {
    sessionStorage.clear();
    const userName = Const.USERNAME_SEGURIDAD;
    const password = Const.PASSWORD_SEGURIDAD;
    const header = new HttpHeaders({
      Authorization: `Basic ${btoa(
        unescape(encodeURIComponent(userName + ':' + password))
      )}`,
    });
    const params = {
      trace: {
        traceId: null,
      },
      payload: {
        usuario: login,
        password: clave,
        grantType: 'password',
      },
    };

    const url = `${Const.API_SEGURIDAD}v1/oauth2/tokens`;
    return this.http
      .post<any>(url, params, {
        headers: header,
        observe: 'response',
      })
      .pipe(
        timeout(2000),
        map((response: any) => {
          const usuario = response.body.payload.usuario;
          // const personaId = response.body.payload.personaId;
          sessionStorage.setItem('currentUser', usuario);
          this.currentUserSubject.next({
            numeroDocumento: Number(usuario),
            token: response.body.payload.accessToken,
          });
          sessionStorage.setItem('token', response.body.payload.accessToken);
          sessionStorage.setItem(
            'userId',
            this.jwtHelper.decodeToken(response.body.payload.accessToken)
              .usuario.usuarioId
          );
          this.valToken = response.body.payload.accessToken;
          return response;
        }),
        catchError((e) => {
          if (e.code === 'PTMP') {
            sessionStorage.setItem('userDocument', login);
            this.router.navigateByUrl('/auth/change-password');
          }
          throw e;
        })
      );
  }

  getApplicationRoles(): Observable<any> {
    this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    const estadoRegistro = 1;
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/roles/query?estadoRegistro=${estadoRegistro}&aplicacionId=${aplicacionId}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        this.applicationRoles = response.payload.items;
        console.log("this.applicationRoles:",this.applicationRoles)

        response.payload.items.forEach((element) => {
          if (element.nombreRol === this.nombreRol) {
            if (this.nombreRol.toUpperCase() === 'GESTOR_GDR') {
              sessionStorage.setItem('orden_rol_id', JSON.stringify(1));
            }
            if (this.nombreRol.toUpperCase() === 'EVALUADO') {
              sessionStorage.setItem('orden_rol_id', JSON.stringify(2));
            }
            if (this.nombreRol.toUpperCase() === 'EVALUADOR') {
              sessionStorage.setItem('orden_rol_id', JSON.stringify(3));
            }
          }
        });

        return response;
      })
    );
  }

  getUserRoles() {
    const userId = sessionStorage.getItem('userId');
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/usuarioRoles/query?usuarioId=${userId}&aplicacionId=${aplicacionId}`;
    return this.http.get<any>(url).pipe(
      map((response: any) => {
        this.userRoles = response.payload.slice();
        if (this.userRoles.length !== 0) {
          const userOk = this.validateRoles(
            this.applicationRoles,
            this.userRoles
          );
          if (userOk) {
            return response;
          } else {
            throw new Error('El usuario no tiene acceso al sistema');
          }
        } else {
          throw new Error('El usuario no tiene acceso al sistema');
        }
      })
    );
  }

  getRoleIdentity() {
    const infoToken = this.jwtHelper.decodeToken(
      sessionStorage.getItem('token')
    );
    const url = `${Const.API_SEGURIDAD}v1/usuarios/obtener`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        usuarioId: infoToken.usuario.usuarioId,
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((response: any) => {
        // sessionStorage.setItem(
        //   'roles',
        //   JSON.stringify(response.payload.roles[0])
        // );
        sessionStorage.setItem(
          'userRoles',
          JSON.stringify(response.payload.roles)
        );
        sessionStorage.setItem('fotoPerfil', response.payload.ruta);
        return response;
      })
    );
  }

  getOptionsMenu() {
    const url = `${Const.API_SEGURIDAD}v1/oauth2/menus`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const menu = response.payload.items;
        menu.map((item) => (item.opened = false));
        this.sidenavService.setMenu(menu);
        return response;
      })
    );
  }

  validateRoles(arrayRoles: any[], arrayRoluser: any[]) {
    const auxRolUserValidos = arrayRoluser.filter(
      (r) => r.estadoRegistro !== '0'
    );
    if (auxRolUserValidos.length === 0) {
      return false;
    }
    sessionStorage.setItem('roles', JSON.stringify(auxRolUserValidos[0]));
    // sessionStorage.setItem('userRoles', JSON.stringify(auxRolUserValidos));
    const rolId = auxRolUserValidos[0].rolId;
    const roles = arrayRoles.map((r) => r.rolId);
    return roles.includes(rolId);
  }

  generatePublicToken(): Observable<any> {
    const userName = Const.USERNAME_SEGURIDAD;
    const password = Const.PASSWORD_SEGURIDAD;
    const codigo = `${btoa(
      unescape(encodeURIComponent(userName + ':' + password))
    )}`;
    const header = {
      headers: new HttpHeaders().set('Authorization', `Basic ${codigo}`),
    };

    const body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        grantType: 'clientCredentials',
      },
    };
    return this.http
      .post(`${Const.API_SEGURIDAD}v1/oauth2/tokens`, body, header)
      .pipe(
        map((response: any) => {
          this.currentUserSubject.next({
            numeroDocumento: null,
            token: response.payload.accessToken,
          });
          return response;
        })
      );
  }

  getDataReniec() {
    const numeroDni = sessionStorage.getItem('currentUser');
    let tipoDoc = null;
    numeroDni.length === 8 ? (tipoDoc = 1) : (tipoDoc = 4);
    const url = `${Const.API_PERSONA}v1/personas/documentoQuery?tipoDocumento=${tipoDoc}&numeroDocumento=${numeroDni}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const persona = response.payload.personaNatural;
        const tramaPersona = {
          numeroDocumento: Number(sessionStorage.getItem('currentUser')),
          personaId: response.payload.persona.personaId,
          nombres: persona.nombres,
          apellidoPaterno: persona.apellidoPaterno,
          apellidoMaterno: persona.apellidoMaterno || '',
          direccion: response.payload.direcciones[0]?.direccionCompleta || null,
          nombreCompleto:
            persona.apellidoPaterno +
            ' ' +
            (persona.apellidoMaterno ? persona.apellidoMaterno + ' ' : '') +
            persona.nombres,
          correo: response.payload.correos ? response.payload.correos[0].correo : null,
          telefono: response.payload.telefono ? response.payload.telefonos[0].numeroTelefono : null,
          idTelefono: response.payload.telefono ? response.payload.telefonos[0].telefonoId : null,
          idCorreo: response.payload.correos ? response.payload.correos[0].correoId : null,
          anexo: response.payload.telefono ? response.payload.telefonos[0].numeroAnexo : null,
          token: sessionStorage.getItem('token'),
          entidadId:
            this.jwtHelper.decodeToken(sessionStorage.getItem('token')).entidad
              ?.entidadId || null,
          rolId: JSON.parse(sessionStorage.getItem('roles')).rolId || null,
        };
        this.currentUserSubject.next(tramaPersona);
        sessionStorage.setItem('persona', JSON.stringify(tramaPersona));
        return tramaPersona;
      }),
      timeout(5000),
      catchError((e) => {
        throw e;
      })
    );
  }

  forgotPassword(numeroDocumento: string): Observable<any> {
    this.currentUserSubject.next(null);

    const userName = Const.USERNAME_SEGURIDAD;
    const password = Const.PASSWORD_SEGURIDAD;
    const codigo = `${btoa(
      unescape(encodeURIComponent(userName + ':' + password))
    )}`;
    const url = `${Const.API_SEGURIDAD}v1/oauth2/password/reset`;

    const headers: HttpHeaders = new HttpHeaders({
      'Content-Type': 'application/json',
      Authorization: `Basic ${codigo}`,
    });

    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        usuario: numeroDocumento,
        urlSistema: `${window.location.href.split('auth')[0]}auth/login`,
      },
    };
    return this.http.put(url, tramaEnvio, { headers }).pipe(
      timeout(5000),
      catchError((e) => {
        throw e;
      })
    );
  }

  changePassword(password: string, newPassword: string): Observable<any> {
    const user = Const.USERNAME_SEGURIDAD;
    const pass = Const.PASSWORD_SEGURIDAD;
    const header = {
      headers: new HttpHeaders().set(
        'Authorization',
        `Basic ${btoa(unescape(encodeURIComponent(user + ':' + pass)))}`
      ),
    };
    const params = {
      trace: {
        traceId: null,
      },
      payload: {
        usuario: sessionStorage.getItem('userDocument') || null,
        passwordActual: password,
        passwordNuevo: newPassword,
      },
    };
    return this.http.put(
      `${Const.API_SEGURIDAD}v1/oauth2/password/cambio`,
      params,
      header
    );
  }

  logout(): void {
    sessionStorage.clear();
    this.currentUserSubject.next(null);
    this.router.navigateByUrl('/auth');
  }

  clearUser(): void {
    this.currentUserSubject.next(null);
  }

  /*isGestor(): boolean {
    return null;
    this.getRoles().find(item => item.rolId === Const.R_GESTOR_ORH) != null || null;
  }*/

  /*isCoordinador(): boolean {
    return null;
    this.getRoles().find(item => item.rolId === Const.R_COORDINADOR) != null || null;
  }*/

  /*isGestorAndCoord(): boolean {
    return null;
    this.getRoles().filter(item => item.rolId === Const.R_GESTOR_ORH || item.rolId === Const.R_COORDINADOR).length === 2 || null;
  }*/

  getRoles(): UserRoles[] {
    return JSON.parse(sessionStorage.getItem('userRoles'));
  }

  getUserRolesPerfil() {
    const userId = sessionStorage.getItem('userId');
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/usuarioRoles/query?usuarioId=${userId}&aplicacionId=${aplicacionId}`;
    return this.http.get<any>(url).pipe(
      map((response: any) => {
        this.userRoles = response.payload.slice();
        if (this.userRoles.length !== 0) {
          const userOk = this.validateRoles(
            this.applicationRoles,
            this.userRoles
          );
          if (userOk) {
            return response;
          } else {
            throw new Error('El usuario no tiene acceso al sistema');
          }
        } else {
          throw new Error('El usuario no tiene acceso al sistema');
        }
      })
    );
  }

  /*********************************************************************************************/
  getEntidad(): Observable<any> {
    if (this.valToken) {
      const url = `${Const.API_SEGURIDAD}v1/usuario/usuarioId/entidades`;
      return this.http.get<any>(url).pipe(
        map((response) => {
          return response;
        })
      );
    }
  }

  login3(login: string, clave: string): Observable<any> {
    this.currentUserSubject.next(null);
    return this.GeneraTokenUserPass(login, clave);
  }

  GeneraTokenUserPass(login: string, clave: string): Observable<any> {
    sessionStorage.clear();
    const userName = Const.USERNAME_SEGURIDAD;
    const password = Const.PASSWORD_SEGURIDAD;
    const header = new HttpHeaders({
      Authorization: `Basic ${btoa(
        unescape(encodeURIComponent(userName + ':' + password))
      )}`,
    });
    const params = {
      trace: {
        traceId: null,
      },
      payload: {
        usuario: login,
        password: clave,
        grantType: 'password',
      },
    };

    const url = `${Const.API_SEGURIDAD}v1/oauth2/tokens`;
    return this.http
      .post<any>(url, params, {
        headers: header,
        observe: 'response',
      })
      .pipe(
        timeout(2000),
        map((response: any) => {
          const usuario = response.body.payload.usuario;
          // const personaId = response.body.payload.personaId;
          sessionStorage.setItem('currentUser', usuario);
          this.currentUserSubject.next({
            numeroDocumento: Number(usuario),
            token: response.body.payload.accessToken,
          });
          sessionStorage.setItem('token', response.body.payload.accessToken);
          sessionStorage.setItem(
            'userId',
            this.jwtHelper.decodeToken(response.body.payload.accessToken)
              .usuario.usuarioId
          );
          this.valToken = response.body.payload.accessToken;
          return response;
        }),
        catchError((e) => {
          if (e.code === 'PTMP') {
            sessionStorage.setItem('userDocument', login);
            this.router.navigateByUrl('/auth/change-password');
          }
          throw e;
        })
      );
  }
  getEntidades(token: string): Observable<any> {
    this.entidadesIds = null;
    let val = '';
    if (this.valToken) {
      const url = `${Const.API_SEGURIDAD}v1/usuario/usuarioId/entidades`;
      return this.http.get<any>(url).pipe(
        map((response) => {
          response.payload.items.forEach((item) => {
            val += item.entidadId + ',';
          });
          this.entidadesIds = val.slice(0, val.length - 1);
          return this.entidadesIds;
        })
      );
    }
  }

  getListaEntidades(entidadesIds: string): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/entidad/filter/?listId=${entidadesIds}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        return response;
      })
    );
  }

  getRolEntidad(entidadId, rolId,nombreRolId?:any): Observable<any> {
    this.rolId = rolId;
    const userId = sessionStorage.getItem('userId');
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/usuarioRoles/query?usuarioId=${userId}&aplicacionId=${aplicacionId}&entidadId=${entidadId}`;
    return this.http.get<any>(url).pipe(
      map((response: any) => {
        this.userRoles = response.payload.slice();
        if (this.userRoles.length !== 0) {
          if (this.userRoles.length > 1 && this.rolId == null) {
            return this.userRoles;
          } else {
            if (this.rolId !== null) {
              return this.getApplicationRolesEntidad(nombreRolId).subscribe((item) => {
                this.applicationRoles = item.payload.items;
                const userOk = this.validaRoles(
                  this.applicationRoles,
                  this.userRoles,
                  this.rolId
                );
                if (userOk) {
                  return response;
                } else {
                  throw new Error('El usuario no tiene acceso al sistema');
                }
              });
            } else {
              let idRol = this.userRoles[0].rolId;
              return this.getApplicationRolesEntidad().subscribe((item) => {
                this.applicationRoles = item.payload.items;
                const userOk = this.validaRoles(
                  this.applicationRoles,
                  this.userRoles,
                  idRol
                );
                if (userOk) {
                  return response;
                } else {
                  throw new Error('El usuario no tiene acceso al sistema');
                }
              });
            }
          }
        } else {
          throw new Error('El usuario no tiene acceso al sistema');
        }
      })
    );
  }

  getApplicationRolesEntidad(nombreRolCombo?:any): Observable<any> {
    const estadoRegistro = 1;
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/roles/query?estadoRegistro=${estadoRegistro}&aplicacionId=${aplicacionId}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        this.applicationRoles = response.payload.items;
        console.log("this.applicationRoles:",this.applicationRoles)
        sessionStorage.setItem(
          'RolRector',
          JSON.stringify(
            response.payload.items.find((item: { nombreRol: string }) =>
              item.nombreRol.toLowerCase().includes('rector')
            )
          )

        );

        if(nombreRolCombo!=null && nombreRolCombo!=undefined && nombreRolCombo!='undefined') {
         
          this.applicationRoles.forEach((element) => {
            if (element.nombreRol === nombreRolCombo) {
              if (element.nombreRol.toUpperCase() === 'GESTOR_GDR') {
                sessionStorage.setItem('orden_rol_id', JSON.stringify(1));
              }
              if (element.nombreRol.toUpperCase() === 'EVALUADO') {
                sessionStorage.setItem('orden_rol_id', JSON.stringify(2));
              }
              if (element.nombreRol.toUpperCase() === 'EVALUADOR') {
                sessionStorage.setItem('orden_rol_id', JSON.stringify(3));
              }
            }
          });
        }
        return response;
      })
    );
  }

  

  validaRoles(arrayRoles: any[], arrayRoluser: any[], rol: number) {
    const auxRolUserValidos = arrayRoluser.filter(
      (r) => r.estadoRegistro !== '0'
    );
    if (auxRolUserValidos.length === 0) {
      return false;
    }
    if (rol) {
      const rolAsignado = auxRolUserValidos.find((item) => item.rolId === rol);
      sessionStorage.setItem('roles', JSON.stringify(rolAsignado));
      const rolId = rolAsignado.rolId;
      const roles = arrayRoles.map((r) => r.rolId);
      return roles.includes(rolId);
    } else {
      sessionStorage.setItem('roles', JSON.stringify(auxRolUserValidos[0]));
      // sessionStorage.setItem('userRoles', JSON.stringify(auxRolUserValidos));
      const rolId = auxRolUserValidos[0].rolId;
      const roles = arrayRoles.map((r) => r.rolId);
      return roles.includes(rolId);
    }
  }

  verifyEntityUpdated(idEntidad: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${idEntidad}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        const entidad = response.payload.entidad[0];
        const val = this.currentUserSubject.value;
        val.entidadNombre = entidad.descripcionEntidad;
        this.currentUserSubject.next(val);
        sessionStorage.setItem('persona', JSON.stringify(val));
        sessionStorage.setItem('entidad', JSON.stringify(entidad));
        // this.currentUserSubject.next(...val, '');d
        return entidad.flagActualiza;
      }),
      concatMap((res) => this.getOptionsMenu()),
      concatMap((res) => this.getDataReniec())
    );
  }

  verifyEntityUpdatedV2(idEntidad: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${idEntidad}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        const entidad = response.payload.entidad[0];
        const val = this.currentUserSubject.value;
        val.entidadNombre = entidad.descripcionEntidad;
        this.currentUserSubject.next(val);
        // sessionStorage.setItem('persona', JSON.stringify(val));
        sessionStorage.setItem('entidad', JSON.stringify(entidad));
        // this.currentUserSubject.next(...val, '');d
        return entidad.flagActualiza;
      })
    );
  }
 
  verifyEntityRolUpdated(idEntidad: number, idRol: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${idEntidad}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        const entidad = response.payload.entidad[0];
        const val = this.currentUserSubject.value;
        val.entidadNombre = entidad.descripcionEntidad;
        this.currentUserSubject.next(val);
        sessionStorage.setItem('persona', JSON.stringify(val));
        sessionStorage.setItem('entidad', JSON.stringify(entidad));
        // this.currentUserSubject.next(...val, '');d
        return entidad.flagActualiza;
      }),
      concatMap((res) => this.getOptionsMenuRol(idEntidad, idRol)),
      concatMap((res) => this.getDataReniecByEntidad(idEntidad)), // getDataReniec
      concatMap((res) => this.getCiclo(idEntidad))
    );
  }
 
  cambiarEntidad(idEntidad: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${idEntidad}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        const entidad = response.payload.entidad[0];
        sessionStorage.setItem('entidad', JSON.stringify(entidad));
        // this.currentUserSubject.next(...val, '');d
        return entidad;
      })
    );
  }

  getOptionsMenuRol(idEntidad, idRol) {
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/roles/${idRol}/entidad/${idEntidad}/aplicacion/${aplicacionId}/subMenus`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const menu = response.payload.treeMenusByAplicacion;
        menu.map((item) => (item.opened = false));
        this.sidenavService.setMenu(menu);
        return response;
      })
    );
  }

  getRolId(entidadId: number): Observable<any> {
    const userId = sessionStorage.getItem('userId');
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/usuarioRoles/query?usuarioId=${userId}&aplicacionId=${aplicacionId}&entidadId=${entidadId}`;
    return this.http.get<any>(url).pipe(
      map((response: any) => {
        this.userRoles = response.payload.slice();
        if (this.userRoles.length !== 0) {
          if (this.userRoles.length === 1) {
            return this.userRoles;
          }
        }
      })
    );
  }

  getCiclo(entidadId: number): Observable<any> {
    const url = `${Const.API_PLANIFICACION}v1/ciclos/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          this.toast.showToast(response.status.error.messages[0], 'danger');
        } else {
          if (response.payload.items.length > 0) {
            const ciclo = response.payload.items[0];
            sessionStorage.setItem('ciclo', JSON.stringify(ciclo));
          } else {
            const ciclo = response.payload.items;
            sessionStorage.setItem('ciclo', JSON.stringify(ciclo));
          }
        }
      })
    );
  }

  getCurrentCiclo(): Observable<any> {
    let currentCicle = JSON.parse(sessionStorage.getItem('ciclo'));
    if (currentCicle == null) return of(null);
    else return of(currentCicle.anio);
  }

  getUsuarioId(usuarioId: number): Observable<any> {
    const url = `${Const.API_SEGURIDAD}v1/usuarios/${usuarioId}`;
    return this.http.get<any>(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }
  verifyImage(url: string) {
    let imgUrl = new HttpRequest('HEAD', url, false);
    return this.http.request(imgUrl);
  }

  getDataReniecByEntidad(entidadId: number) {
    const numeroDni = sessionStorage.getItem('currentUser');
    let tipoDoc = null;
    numeroDni.length === 8 ? (tipoDoc = 1) : (tipoDoc = 4);
    const url = `${Const.API_PERSONA}v1/personas/documentoQuery?tipoDocumento=${tipoDoc}&numeroDocumento=${numeroDni}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        const persona = response.payload.personaNatural;
        const tramaPersona = {
          numeroDocumento: Number(sessionStorage.getItem('currentUser')),
          personaId: response.payload.persona.personaId,
          nombres: persona.nombres,
          apellidoPaterno: persona.apellidoPaterno,
          apellidoMaterno: persona.apellidoMaterno || '',
          direccion: response.payload.direcciones[0]?.direccionCompleta || null,
          nombreCompleto:
            persona.apellidoPaterno +
            ' ' +
            (persona.apellidoMaterno ? persona.apellidoMaterno + ' ' : '') +
            persona.nombres,
          correo: response.payload.correos
            ? response.payload.correos[0].correo
            : null,
          telefono: response.payload.telefono
            ? response.payload.telefonos[0].numeroTelefono
            : null,
          idTelefono: response.payload.telefono
            ? response.payload.telefonos[0].telefonoId
            : null,
          idCorreo: response.payload.correos
            ? response.payload.correos[0].correoId
            : null,
          anexo: response.payload.telefono
            ? response.payload.telefonos[0].numeroAnexo
            : null,
          token: sessionStorage.getItem('token'),
          entidadId: entidadId,
          rolId: JSON.parse(sessionStorage.getItem('roles')).rolId || null,
        };
        this.currentUserSubject.next(tramaPersona);
        sessionStorage.setItem('persona', JSON.stringify(tramaPersona));
        return tramaPersona;
      }),
      timeout(5000),
      catchError((e) => {
        throw e;
      })
    );
  }
}
