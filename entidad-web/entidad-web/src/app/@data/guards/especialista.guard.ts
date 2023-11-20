import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { Const } from '../services/const';

@Injectable({
  providedIn: 'root',
})
export class EspecialistaGuard implements CanActivate {
  constructor(public router: Router) {}

  canActivate(): boolean {
    const rol = JSON.parse(sessionStorage.getItem('roles'));
    if (rol.rolId === Const.R_ESPECIALISTA) {
      return true;
    } else {
      return false;
    }
  }
}
