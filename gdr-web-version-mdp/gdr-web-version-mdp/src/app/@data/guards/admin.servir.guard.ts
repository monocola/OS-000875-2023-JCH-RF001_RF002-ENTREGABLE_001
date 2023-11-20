import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';

@Injectable({
  providedIn: 'root',
})
export class AdminServirGuard implements CanActivate {
  constructor(public router: Router) {}

  canActivate(): boolean {
    // const rol = JSON.parse(sessionStorage.getItem('roles'));
    return false;
    /*if (rol.rolId === Const.R_ADMIN_SERVIR) {
      return true;
    } else {
      return false;
    }*/
  }
}
