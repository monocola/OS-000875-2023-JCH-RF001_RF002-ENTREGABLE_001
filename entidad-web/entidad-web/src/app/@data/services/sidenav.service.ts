import { Injectable } from '@angular/core';
import { ItemMenu } from '../model/item-menu';

@Injectable({
  providedIn: 'root',
})
export class SidenavService {
  collapsed = false;

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
