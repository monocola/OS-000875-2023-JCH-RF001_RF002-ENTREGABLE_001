import { Observable } from 'rxjs';
import { UsuarioGestor } from '../../@data/model/usuariosGestores';

export abstract class ConfiguracionRepository {
  abstract getUsuarioRector(): Observable<UsuarioGestor[]>;
  abstract asignaRector(body: any): Observable<any>;
  abstract getListUsuarios(): Observable<any>; /* estadoId?: number */
  abstract getListUsuariosServir(): Observable<any>; /* estadoId?: number */
  abstract getCorreo(personaId: number): Observable<any>;
  abstract notificaUsuarioReg(body: any): Observable<any>;
  abstract asignaRectorEntidad(body: any): Observable<any>;
}
