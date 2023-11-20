import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';

@Injectable({
  providedIn: 'root',
})
export class CoordinadorGuard implements CanActivate {
  constructor(public router: Router) {}

  canActivate(): boolean {
    // const rol = JSON.parse(sessionStorage.getItem('roles'));
    return false;
    /*if (rol.rolId === Const.R_COORDINADOR) {
      return true;
    } else {
      return false;
    }*/
  }
}
