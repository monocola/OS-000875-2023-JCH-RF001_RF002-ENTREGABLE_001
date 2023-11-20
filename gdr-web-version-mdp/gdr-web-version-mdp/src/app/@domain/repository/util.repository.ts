import { Observable } from 'rxjs/Observable';

export abstract class UtilRepository {

  abstract obtenerPdf(uiid: string): any;
  abstract registrarResolucion(body: any, archivo: any): Observable<any>;
  abstract subirFormatoMetas(body: any, archivo: any): Observable<any>;
  abstract obtenerDocumentoPdf(uiid: string): any;
  abstract obtenerDocumentoPdfSinUiid(detUOId: string, personaId: string, cicloId: string): any;
}
