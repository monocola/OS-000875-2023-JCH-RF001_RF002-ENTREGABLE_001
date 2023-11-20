import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivateChild, Router, RouterStateSnapshot } from '@angular/router';

@Injectable({ providedIn: 'root' })
export class RolGuard implements CanActivateChild {
  constructor(
    private router: Router,
  ) {}

  canActivateChild(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ) {
    let menus = JSON.parse(sessionStorage.getItem('menu'));
    let authRoutes = menus.map(item => item.url);
    menus.filter(item => item.subMenu.length !== 0).forEach((item: any) => authRoutes = [...authRoutes, ...item.subMenu.map(item => item.url)]);
    if (state.url.includes('/pages/home')) {
      return true;
    } else if (state.url.includes('/pages') && !authRoutes.includes(state.url.replace('/pages', ''))) {
      this.router.navigate(['/pages/']);
      return false;
    } else {
      return true;
    }
  }
}
