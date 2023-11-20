import { Component, OnInit } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { User } from 'src/app/@data/model/user';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from '../../../@data/services/sidenav.service';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { Ciclos } from '../../../@data/model/ciclos';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { CicloService } from '../../../@data/services/ciclo.service';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { ToastService } from '../../@common-components/toast';
import { DatePipe } from '@angular/common';

@Component({
  selector: 'serv-talento-default-page',
  templateUrl: './default-page.component.html',
  styleUrls: ['./default-page.component.scss'],
})
export class DefaultPageComponent implements OnInit {
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  disabled:boolean = false;

  constructor(
    private authRepository: AuthenticationRepository,
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private maeParametroRepository: MaestraParametroRepository,
    private cicloService: CicloService,
    private toastService: ToastService,
    private datePipe: DatePipe
  ) {}

  cicloDefault: number;
  lstCiclos: Ciclos[] = [];
  estados: MaestraParametro[];
  cicloDefaultDesc: string;

  ngOnInit(): void {
    this.user = this.authRepository.getCurrentUserValue;
    setTimeout(() => {
      this.subscribeEvents();
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    }, 0);

    this.loadCombox();
  }

  loadCombox() {
    const getEstados = this.maeParametroRepository.getMaestraParametro(
      'CICLO_ESTADO'
    );
    let entidad = JSON.parse(sessionStorage.getItem('entidad'));
    console.log("entidad ciclo:",entidad)
    const getListCiclos = this.cicloService.getListCiclo(entidad.entidadId);
    forkJoin([getEstados, getListCiclos]).subscribe(
      (results) => {
        this.estados = results[0];
        this.setCiclo(results[1]);
      },
      (error) => {
        this.toastService.showToast(
          error + ', No se cargo la información correctamente',
          'danger'
        );
      }
    );
  }

  setCiclo(lstCiclo: any []) {

    this.lstCiclos = lstCiclo;

    this.disabled = false;

    if (this.ciclo.length !== 0 && this.ciclo) {

      this.cicloDefault = this.ciclo.cicloId;
      this.cicloDefaultDesc = this.ciclo.cicloId.anioString;
 
      console.log("entreeee")


      if (this.lstCiclos.length > 0) {

      console.log("entreeee listaaaaaaaaa")


        let i = 0;
        lstCiclo.forEach((item) => {

      console.log("foreach")

          this.lstCiclos[i].fechaIni = this.datePipe.transform(
            item.fechaIni,
            'dd/MM/yyyy'
          );
          this.lstCiclos[i].fechaFin = this.datePipe.transform(
            item.fechaFin,
            'dd/MM/yyyy'
          );
          this.lstCiclos[i].anioString = this.lstCiclos[i].anio.toString();
          i += 1;
        });

        console.log("cilocs", this.lstCiclos)

      } else {
        this.cicloDefault = 0;
      }
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
      
      if(this.lstCiclos.length>0) {
        let i = 0;
        lstCiclo.forEach((item) => {

      console.log("foreach")

          this.lstCiclos[i].fechaIni = this.datePipe.transform(
            item.fechaIni,
            'dd/MM/yyyy'
          );
          this.lstCiclos[i].fechaFin = this.datePipe.transform(
            item.fechaFin,
            'dd/MM/yyyy'
          );
          this.lstCiclos[i].anioString = this.lstCiclos[i].anio.toString();
          i += 1;
        });
      }
    }
    console.log("ciclosssss", this.lstCiclos);

    if(sessionStorage.getItem('cicloIdMonitoreo')) {
      this.cicloDefault = Number(sessionStorage.getItem('cicloIdMonitoreo'));
      this.disabled = true;
      this.cambiaCiclo(this.cicloDefault);
    }
  

  }

  subscribeEvents() {
    this.sidebarService.onCompact().subscribe(() => {
      this.sidenavService.collapsed = true;
    });
    this.sidebarService.onExpand().subscribe(() => {
      this.sidenavService.collapsed = false;
    });
  }

  cambiaCiclo(events) {
    if (this.lstCiclos) {
      const ciclo = this.lstCiclos.find((item) => item.cicloId === events);
      sessionStorage.setItem('ciclo', JSON.stringify(ciclo));
      this.ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    }
  }
}
