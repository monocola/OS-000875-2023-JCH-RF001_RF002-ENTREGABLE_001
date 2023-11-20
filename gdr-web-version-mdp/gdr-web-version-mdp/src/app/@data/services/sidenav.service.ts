import { Injectable } from '@angular/core';
import { ItemMenu } from '../model/item-menu';
import { Subject } from 'rxjs';

@Injectable({
  providedIn: 'root',
})
export class SidenavService {
  collapsed = false;
  public sideNavState$: Subject<boolean> = new Subject();

  constructor() {
    this.verifyMenu();
  }

  public menu: ItemMenu[];

  setMenu(items: ItemMenu[]) {
    this.menu = items;
    sessionStorage.setItem('menu', JSON.stringify(this.menu));
  }

  verifyMenu() {
    if (sessionStorage.getItem('menu')) {
      const items: ItemMenu[] = JSON.parse(sessionStorage.getItem('menu'));
      this.setMenu(items);
    }
  }
}
