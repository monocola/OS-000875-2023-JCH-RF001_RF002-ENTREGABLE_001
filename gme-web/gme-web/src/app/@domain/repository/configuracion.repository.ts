import { Observable } from 'rxjs';

export abstract class ConfiguracionRepository {
  abstract notificaUsuarioReg(body: any): Observable<any>;
  abstract enviaCorreoMasivo(body: any): Observable<any>;
}
