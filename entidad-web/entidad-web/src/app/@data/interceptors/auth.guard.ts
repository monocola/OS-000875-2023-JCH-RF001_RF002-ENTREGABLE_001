import { Injectable } from '@angular/core';
import {
  ActivatedRouteSnapshot,
  CanActivate,
  Router,
  RouterStateSnapshot,
} from '@angular/router';
import { AuthenticationRepository } from '../../@domain/repository/authentication.repository';
import { JwtHelperService } from '@auth0/angular-jwt';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Injectable({ providedIn: 'root' })
export class AuthGuard implements CanActivate {
  constructor(
    private router: Router,
    private authenticationService: AuthenticationRepository,
    private toastService: ToastService
  ) {}

  canActivate(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ) {
    const currentUser = this.authenticationService
      .getCurrentUserValue;
    if (currentUser) {
      const helper = new JwtHelperService();
      const token = sessionStorage.getItem('token');
      if (helper.isTokenExpired(token)) {
        this.router.navigate(['/auth/']);
        setTimeout(() => {
          this.toastService.showToast(
            'Tiempo expirado, por favor inicie sesión nuevamente',
            'primary'
          );
        }, 100);
        return false;
      } else {
        return true;
      }
    } else {
      this.router.navigate(['/auth/']);
      setTimeout(() => {
        this.toastService.showToast(
          'Por favor inicie sesión',
          'primary'
        );
      }, 100);
      return false;
    }
  }
}
