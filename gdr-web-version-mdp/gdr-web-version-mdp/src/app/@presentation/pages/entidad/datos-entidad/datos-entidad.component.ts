import { Component, OnInit,  Inject, NgZone, PLATFORM_ID, ViewChild  } from '@angular/core';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { User } from '../../../../@data/model/user';
// /src/app/@data/model/user';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from '../../../../@data/services/sidenav.service';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { MatDialog } from '@angular/material/dialog';
import { ModalEditarComponent } from '../modal-editar/modal-editar.component';
import { ServidoresCivilesGraficosDonats } from 'src/app/@data/model/servidoresCivilesGraficosDonats';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { ChartOptions } from '../entidad.component';
import { ChartComponent } from 'ng-apexcharts';
import { Entity, ResumenServidoresCiviles } from 'src/app/@data/model/entity';
import { GenericoDTO } from 'src/app/@data/model/graficoGenerico';
import { forkJoin } from 'rxjs';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'gme-web-datos-entidad',
  templateUrl: './datos-entidad.component.html',
  styleUrls: ['./datos-entidad.component.scss'],
})
export class DatosEntidadComponent implements OnInit {

  
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  entity: Entity;

  imgEntidad = './assets/images/logo.png';
  name = 'Angular';

  constructor(
    private authRepository: AuthenticationRepository,
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private dialog: MatDialog,
  ) {}

  ngOnInit(): void {
    this.user = this.authRepository.getCurrentUserValue;
    setTimeout(() => {
      this.subscribeEvents();
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    }, 0);
    this.listEntidad();
  }

  listEntidad() {
    //  const entidad =  this.entidadService.getListarEntidad();
    this.entity = JSON.parse(sessionStorage.getItem('entidad'));
    console.log('entidad :', this.entity);
    if (this.entity !== null && this.entity.logo !== null) {
      this.imgEntidad = Const.API_FILE_SERVER + this.entity.logo;
    }
  }

  openModalRegister() {}

  editar(editionMode: boolean = true) {
    const registerDialog = this.dialog.open(ModalEditarComponent, {
      data: {
        editionMode,
        dataToEdit: this.entity,
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      // this.dataToEdit = null;
      if (res) {
        this.listEntidad();
      }
    });
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
