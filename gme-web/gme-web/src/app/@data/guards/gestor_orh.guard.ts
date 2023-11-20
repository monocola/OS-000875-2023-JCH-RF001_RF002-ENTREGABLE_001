import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';

@Injectable({
  providedIn: 'root',
})
export class GestorOrhGuard implements CanActivate {
  constructor(public router: Router) {}

  canActivate(): boolean {
    // const rol = JSON.parse(sessionStorage.getItem('roles'));
    return false;
    /*if (rol.rolId === Const.R_GESTOR_ORH) {
      return true;
    } else {
      return false;
    }*/
  }
}
