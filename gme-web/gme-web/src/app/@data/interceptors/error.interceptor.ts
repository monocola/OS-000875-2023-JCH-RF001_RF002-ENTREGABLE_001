import { Injectable } from '@angular/core';
import {
  HttpEvent,
  HttpHandler,
  HttpInterceptor,
  HttpRequest,
} from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';

@Injectable()
export class ErrorInterceptor implements HttpInterceptor {
  constructor(
    private authenticationService: AuthenticationRepository
  ) {}

  intercept(
    request: HttpRequest<any>,
    next: HttpHandler
  ): Observable<HttpEvent<any>> {
    return next.handle(request).pipe(
      catchError((err) => {
        if (err.status === 401) {
          this.authenticationService.logout();
        }
        if (err.statusText === 'Unknown Error') {
          const error = {
            message: '',
          };
          error.message = 'El servidor no se encuentra disponible';
          return throwError(error);
        } else if (err.status === 405) {
          console.log(err.message);
          let error = { message: 'El servidor no se encuentra disponible'};
          return throwError(error);
        } else {
          const error = err.error?.status.error
            ? err.error?.status.error
            : err.message || err.error.message || err.statusText;
          error.message = error.messages[0] || error;
          return throwError(error);
        }
      })
    );
  }
}
