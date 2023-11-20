import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { Const } from '../services/const';

@Injectable({
  providedIn: 'root',
})
export class SuperAdminEntidadGuard implements CanActivate {
  constructor(public router: Router) {}

  canActivate(): boolean {
    const rol = JSON.parse(sessionStorage.getItem('roles'));
    if (rol.rolId === Const.R_SUPER_ADMIN_ENTIDAD) {
      return true;
    } else {
      this.router.navigateByUrl ('home');
      return false;
    }
  }
}
