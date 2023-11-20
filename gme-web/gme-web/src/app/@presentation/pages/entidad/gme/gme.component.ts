import { Component, OnInit,  Inject, NgZone, PLATFORM_ID, ViewChild  } from '@angular/core';
import { AuthenticationRepository } from '../../../../../../src/app/@domain/repository/authentication.repository';
import { User } from '../../../../../../src/app/@data/model/user';
// /src/app/@data/model/user';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from '../../../../../app/@data/services/sidenav.service';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'gme-web-gme',
  templateUrl: './gme.component.html',
})
export class GmeComponent implements OnInit {
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  dataToEdit;
  
  constructor(
    private authRepository: AuthenticationRepository,
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private dialog: MatDialog,
    private zone: NgZone
  ) {

  }

  ngOnInit(): void {
  

 
  }


}
