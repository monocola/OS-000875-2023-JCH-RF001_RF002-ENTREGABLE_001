import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { Const } from '../services/const';

@Injectable({
  providedIn: 'root',
})
export class ConfiguradorGuard implements CanActivate {
  constructor(public router: Router) {}

  canActivate(): boolean {
    const rol = JSON.parse(sessionStorage.getItem('roles'));
    if (rol.rolId === Const.R_CONFIGURADOR) {
      return true;
    } else {
      return false;
    }
  }
}
