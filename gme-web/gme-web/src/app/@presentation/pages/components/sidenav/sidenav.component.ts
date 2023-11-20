import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { SidenavService } from 'src/app/@data/services/sidenav.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { NbSidebarService } from '@nebular/theme';

@Component({
  selector: 'gme-web-sidenav',
  templateUrl: './sidenav.component.html',
  styleUrls: ['./sidenav.component.scss'],
})
export class SidenavComponent implements OnInit {
  toggleNgModel = false;
  etiqueta: string = 'Fijar Barra de Menú';
  checked = null;
  public sideNavState: boolean = false;

  constructor(
    private authenticationService: AuthenticationRepository,
    public sidenavService: SidenavService,
    public sidebarService: NbSidebarService,
    public router: Router
  ) {}

  ngOnInit(): void {
    this.sidenavService.collapsed = false;
  }

  clickMenu(index) {
    const menu = this.sidenavService.menu.slice(0);
    if (this.sidenavService.collapsed) {
      this.router.navigateByUrl('/pages/' + menu[index].subMenu[0].url);
      menu.forEach((m) => (m.opened = false));
      menu[index].opened = true;
      this.clickItem(index);
    } else {
      menu[index].opened = !menu[index].opened;
    }
    this.sidenavService.setMenu(menu);
  }

  clickItem(indexPadre) {
    const menu = this.sidenavService.menu.slice(0);
    menu.forEach((m) => (m.childSelected = false));
    menu[indexPadre].childSelected = true;
    this.sidenavService.setMenu(menu);
  }

  logout() {
    this.authenticationService.logout();
  }

  clickBar() {
    if (!this.toggleNgModel) {
      this.sidebarService.compact();
    } else {
      this.sidebarService.expand();
    }
  }

  onSinenavToggle() {
    this.sideNavState = !this.sideNavState;
        setTimeout(() => {
    if ( this.toggleNgModel ) {
        if (this.sideNavState) {
          this.sidebarService.compact();
        } else {
          this.sidebarService.expand();
      }
    }
    }, 100);
    this.sidenavService.sideNavState$.next(this.sideNavState);
  }

}
