import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ImplementacionRepository } from 'src/app/@domain/repository/implementacion.repository';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { TypeDocument } from '../model/typeDocument';
import { ApiEntidad, ApiPlaniEntidad } from '../model/entidad';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { EntidadGestor } from '../model/entidadGestor';
import { ItemMenu } from '../model/item-menu';
import { SidenavService } from './sidenav.service';
import { PermissionRepository } from '../../@domain/repository/permission.repository';
import { User } from '../model/user';

@Injectable({
  providedIn: 'root',
})
export class PermissionService implements PermissionRepository {

  constructor(
  ) {
    if (PermissionService.permission == null)
      sessionStorage.setItem('permission', JSON.stringify(PermissionLevel.READANDWRITE));
  }

  public get isReadAndWrite(): boolean {
      return PermissionService.permission === PermissionLevel.READANDWRITE;
  }
  public get isOnlyRead(): boolean {
      return PermissionService.permission === PermissionLevel.READ;
  }

  public setPermission( permission: PermissionLevel) {
    sessionStorage.setItem('permission', JSON.stringify(permission));
  }
  private static get permission(): PermissionLevel {
    let result = JSON.parse(sessionStorage.getItem('permission'));
    return result != null ? result : PermissionLevel.READANDWRITE;
  }
}

export enum PermissionLevel {
  READ,
  READANDWRITE
}

