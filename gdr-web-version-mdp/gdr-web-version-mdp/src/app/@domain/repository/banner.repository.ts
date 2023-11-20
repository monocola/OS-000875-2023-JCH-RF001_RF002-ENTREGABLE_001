import { HttpEvent } from '@angular/common/http';
import { Observable } from 'rxjs';

export abstract class BannerRepository {
  abstract saveImagenes(file: File, urlWeb: string): Observable<HttpEvent<any>>;
  abstract getImagenes(): Observable<any>;
  abstract eliminarImage(bannerId: number): Observable<any>;
  abstract updateUrlWeb(bannerId: number, urlWeb: string): Observable<any>;
  abstract updateOrden(bannerId: number, orden: number): Observable<any>;
}
