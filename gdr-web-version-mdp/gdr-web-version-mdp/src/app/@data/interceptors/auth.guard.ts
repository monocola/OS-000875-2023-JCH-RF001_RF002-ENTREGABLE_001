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
import { ImplementacionService } from '../services/implementacion.service';

@Injectable({ providedIn: 'root' })
export class AuthGuard implements CanActivate {
  constructor(
    private router: Router,
    private authenticationService: AuthenticationRepository,
    private toastService: ToastService,
    private implementacionService: ImplementacionService,
  ) {}

  canActivate(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ) {
    const currentUser = this.authenticationService
      .getCurrentUserValue;
    if (currentUser) {
      let authRoutes = [];
      let menus = JSON.parse(sessionStorage.getItem('menu'));
      authRoutes = menus.map(item => item.url);
      menus.filter(item => item.subMenu.length !== 0).forEach((item: any) => authRoutes = [...authRoutes, ...item.subMenu.map(item => item.url)]);
      const helper = new JwtHelperService();
      const token = sessionStorage.getItem('token');
      if (helper.isTokenExpired(token)) {
        this.implementacionService.restoreMenu();
        this.router.navigate(['/auth/']);
        setTimeout(() => {
          this.toastService.showToast(
            'Tiempo expirado, por favor inicie sesión nuevamente',
            'primary'
          );
        }, 100);
        return false;
      } else if (state.url.includes('/pages/home')) {
        return true;
      } else {
        if (state.url.includes('/pages') && !authRoutes.includes(state.url.replace('/pages', ''))) {
          this.router.navigate(['/pages/']);
          return false;
        } else {
          return true;
        }
      }
    } else {
      this.implementacionService.restoreMenu();
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
