import { HttpClient, HttpEvent, HttpRequest } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { BannerRepository } from 'src/app/@domain/repository/banner.repository';
import { Const } from './const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { map } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';

@Injectable({
  providedIn: 'root',
})
export class BannerService extends BannerRepository {
  responseError = {
    trace: { traceId: null },
    status: {
      success: null,
      error: { code: '', httpCode: '', messages: null },
    },
    payload: null,
  };
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  saveImagenes(file: File, urlWeb: string): Observable<HttpEvent<any>> {
    const formData: FormData = new FormData();
    formData.append('file', file);
    
    const req = new HttpRequest(
      'POST',
      `${Const.API_ENTIDAD}v1/banner?urlWeb=${urlWeb}`,
      formData,
      {
        reportProgress: true,
        responseType: 'json',
      }
    );

    return this.http.request(req);
  }

  getImagenes(): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/banners`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.banners;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  eliminarImage(bannerId: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/banner/${bannerId}`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  updateUrlWeb(bannerId: number, urlWeb: string): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/banner/${bannerId}?urlWeb=${urlWeb}`;
    return this.http.put(url, '').pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  updateOrden(bannerId: number, orden: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/banner/${bannerId}/${orden}`;
    return this.http.put(url, '').pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.mensaje;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
}
