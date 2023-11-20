import { Component, OnInit } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { User } from 'src/app/@data/model/user';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from '../../../@data/services/sidenav.service';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';

@Component({
  selector: 'gme-web-default-page',
  templateUrl: './default-page.component.html',
  styleUrls: ['./default-page.component.scss']
})

export class DefaultPageComponent implements OnInit {

  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';

  constructor(
    private authRepository: AuthenticationRepository,
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService
  ) { }

  ngOnInit(): void {
    this.user = this.authRepository.getCurrentUserValue;
    setTimeout(() => {
      this.subscribeEvents();
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    }, 0);
  }

  subscribeEvents() {
    this.sidebarService.onCompact().subscribe(() => {
      this.sidenavService.collapsed = true;
    });
    this.sidebarService.onExpand().subscribe(() => {
      this.sidenavService.collapsed = false;
    });
  }

}
