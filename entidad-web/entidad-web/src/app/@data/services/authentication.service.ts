import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { BehaviorSubject, Observable } from 'rxjs';
import { catchError, concatMap, map, timeout } from 'rxjs/operators';
import { Const } from './const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

import { JwtHelperService } from '@auth0/angular-jwt';
import { Router } from '@angular/router';
import { SidenavService } from './sidenav.service';
import { User } from '../model/user';
import { UserRoles } from '../model/userRoles';

@Injectable({ providedIn: 'root' })
export class AuthenticationService extends AuthenticationRepository {
  private currentUserSubject: BehaviorSubject<User>;
  public currentUser: Observable<User>;
  private applicationRoles = [];
  private userRoles = [];

  constructor(
    private http: HttpClient,
    private jwtHelper: JwtHelperService,
    private router: Router,
    private sidenavService: SidenavService
  ) {
    super();
    // Falta el endpoint de obtener data de usuario por mientras sera del api persona
    this.currentUserSubject = new BehaviorSubject<User>(
      JSON.parse(sessionStorage.getItem('persona'))
    );
    this.currentUser = this.currentUserSubject.asObservable();
  }

  public get getCurrentUserValue(): User {
    return this.currentUserSubject.value;
  }

  login(login: string, clave: string): Observable<any> {
    this.currentUserSubject.next(null);
    return this.verifyCredentials(login, clave).pipe(
      concatMap((res) => this.getApplicationRoles()),
      concatMap((res) => this.getUserRoles()),
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
    const estadoRegistro = 1;
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/roles/query?estadoRegistro=${estadoRegistro}&aplicacionId=${aplicacionId}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        this.applicationRoles = response.payload.items;
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
        sessionStorage.setItem('userRoles', JSON.stringify(response.payload.roles));
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

  getUserData() {
    const userId = sessionStorage.getItem('userId');
    const url = `${Const.API_SEGURIDAD}v1/usuarios/${userId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response;
      })
    );
  }

  getUserCorreo(correo:string) {
    const url = `${Const.API_ENTIDAD}v1/entidad/not-cambio-pass/${correo}`;
    return this.http.get(url).pipe(
      map((response: any) => {
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
          correo: response.payload.correos[0].correo || null,
          telefono: response.payload.telefonos[0].numeroTelefono || null,
          idTelefono: response.payload.telefonos[0].telefonoId || null,
          idCorreo: response.payload.correos[0].correoId || null,
          anexo: response.payload.telefonos[0].numeroAnexo || null,
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

  verifyEntityUpdated(idEntidad: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/entidad/${idEntidad}`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        const entidad = response.payload.entidad[0];
        const val = this.currentUserSubject.value;
        val.entidadNombre = entidad.descripcionEntidad;
        this.currentUserSubject.next(val);
        sessionStorage.setItem('persona', JSON.stringify(val));
        // this.currentUserSubject.next(...val, '');d
        return entidad.flagActualiza;
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

  changePasswordPerfil(password: string, newPassword: string): Observable<any> {

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
        usuario: sessionStorage.getItem('currentUser') || null,
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

  isGestor(): boolean {
    return this.getRoles()
      .find(item => item.rolId === Const.R_GESTOR_ORH) != null || null;
  }

  isCoordinador(): boolean {
    return this.getRoles()
      .find(item => item.rolId === Const.R_COORDINADOR) != null || null;
  }

  isSuperAdminEntidad(): boolean {
    return this.getRoles()
      .find(item => item.rolId === Const.R_SUPER_ADMIN_ENTIDAD) != null || null;
  }

  isAdminEntidad(): boolean {
    return this.getRoles()
      .find(item => item.rolId === Const.R_ADMIN_ENTIDAD) != null || null;
  }

  isGestorAndCoord(): boolean {
    return this.getRoles()
      .filter(item => item.rolId === Const.R_GESTOR_ORH || item.rolId === Const.R_COORDINADOR)
      .length === 2 || null;
  }

  getRoles(): UserRoles[] {
    return JSON.parse(sessionStorage.getItem('userRoles'));
  }
}
