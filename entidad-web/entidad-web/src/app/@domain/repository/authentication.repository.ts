import { User } from "../../@data/model/user";
import { Observable } from "rxjs";


export abstract class AuthenticationRepository {

  abstract get getCurrentUserValue(): User;
  abstract login(login: string, clave: string): Observable<any>;
  abstract logout(): void;
  abstract forgotPassword(numeroDocumento: string): Observable<any>;
  abstract changePassword(password: string, newPassword: string): Observable<any>;
  abstract changePasswordPerfil(password: string, newPassword: string): Observable<any>;
  abstract verifyEntityUpdated(id: number): Observable<any>;
  abstract generatePublicToken(): Observable<any>;
  abstract clearUser();
  abstract isGestor(): boolean;
  abstract isCoordinador(): boolean;
  abstract isSuperAdminEntidad(): boolean;
  abstract isAdminEntidad(): boolean;
  abstract isGestorAndCoord(): boolean;
  abstract getUserData (): Observable<any>;
  abstract getUserCorreo (correo: string): Observable<any>;
  // abstract verificaCodigo(iduser: string, cod: string): Observable<boolean>;

}
