import { Observable } from 'rxjs/Observable';

export abstract class SolicitudExternaRepository {

  abstract subirDocumentoSolicitudExterna (body: any, archivo: any): Observable<any>;
  abstract enviarSolicitudExterna (request: any): Observable<any>;
  abstract getAnio(): Observable<any>;
  abstract getSolicitudExt(body: any): Observable<any[]>;
  abstract getSolicitudExtId(Id: number): Observable<any>;
  abstract rechazaSolicitudExt(id: number): Observable<any>;
  abstract observaSolicitudExt(id: number, obs: any): Observable<any>;
  abstract validaSolicitudExt(id: number): Observable<any>;
}
