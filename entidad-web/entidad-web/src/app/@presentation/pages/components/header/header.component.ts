import { Component, OnInit } from '@angular/core';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { faUnlockAlt } from '@fortawesome/free-solid-svg-icons';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from 'src/app/@data/services/sidenav.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ModalCambioContraComponent } from '../modal-cambio-contra/modal-cambio-contra.component';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { ToastService } from '../../../@common-components/toast';
@Component({
  selector: 'serv-talento-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.scss'],
})
export class HeaderComponent implements OnInit {
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  icon = faUnlockAlt;
  nombreRol = '';

  constructor(
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private dialog: MatDialog,
    public router: Router,
    private toast: ToastService,
  ) {}

  ngOnInit(): void {
    setTimeout(() => {
      this.subscribeEvents();
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    }, 0);
  }

  logout() {
    this.authenticationService.logout();
  }

  clickBar() {
    !this.sidenavService.collapsed
      ? this.sidebarService.compact()
      : this.sidebarService.expand();
  }

  subscribeEvents() {
    this.sidebarService.onCompact().subscribe(() => {
      this.sidenavService.collapsed = true;
    });
    this.sidebarService.onExpand().subscribe(() => {
      this.sidenavService.collapsed = false;
    });
  }

  changepswd() {
    const modal = this.dialog.open(ModalCambioContraComponent, {
      width: '450px',
      data: {
        titulo: 'Cambio de contraseña',
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        this.toast.showToast(
          'La contraseña ha sido cambiada con éxito',
          'success',
          ''
        );
      }
    });
  }
}
