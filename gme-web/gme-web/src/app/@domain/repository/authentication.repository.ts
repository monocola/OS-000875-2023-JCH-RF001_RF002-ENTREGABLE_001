import { User } from "../../@data/model/user";
import { Observable } from "rxjs";


export abstract class AuthenticationRepository {

  abstract get getCurrentUserValue(): User;
  // abstract login(login: string, clave: string): Observable<any>;
  abstract logout(): void;
  abstract forgotPassword(numeroDocumento: string): Observable<any>;
  abstract changePassword(password: string, newPassword: string): Observable<any>;
  abstract verifyEntityUpdated(id: number): Observable<any>;
  abstract verifyEntityUpdatedV2(id: number): Observable<any>;

  abstract generatePublicToken(): Observable<any>;
  abstract clearUser();
  // abstract isGestor(): boolean;
  // abstract isCoordinador(): boolean;
  // abstract isGestorAndCoord(): boolean;
  // abstract verificaCodigo(iduser: string, cod: string): Observable<boolean>;

  /****************************/
  abstract getEntidad(token: string): Observable<any>;
  abstract login3(login: string, clave: string): Observable<any>;
  abstract getEntidades(token: string): Observable<any[]>;
  abstract getListaEntidades(entidadesId: string): Observable<any>;
  abstract getRolEntidad(entidadId: number, rolId: number): Observable<any>;
  abstract verifyEntityRolUpdated(id: number, idRol: number): Observable<any>;
  abstract getOptionsMenuRol(entidadId: number, idRol: number): Observable<any>;
  abstract getRolId(entidadId: number): Observable<any>;
  abstract verifyImage(url: string): Observable<any>;
  abstract getEncryptSolicitudExtId(Id: number): Observable<any>;
  abstract getUsuarioId(usuarioId: number): Observable<any>;
}
