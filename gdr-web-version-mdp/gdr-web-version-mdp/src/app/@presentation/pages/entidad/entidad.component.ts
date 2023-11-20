import {
  Component,
  OnInit,
  Inject,
  NgZone,
  PLATFORM_ID,
  ViewChild,
  AfterContentChecked,
  ChangeDetectorRef,
} from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { User } from 'src/app/@data/model/user';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from '../../../@data/services/sidenav.service';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { MatDialog } from '@angular/material/dialog';
import { ModalEditarComponent } from './modal-editar/modal-editar.component';
// amCharts imports
import { ApexTooltip, ChartComponent } from 'ng-apexcharts';
import {
  ApexNonAxisChartSeries,
  ApexResponsive,
  ApexChart,
  ApexDataLabels,
  ApexLegend,
  ApexStroke,
  ApexPlotOptions,
  ApexStates,
  ApexTheme,
  ApexTitleSubtitle,
} from 'ng-apexcharts';
import { EntidadRepository } from 'src/app/@domain/repository/entidad.repository';
import { forkJoin } from 'rxjs';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { Entity, ResumenServidoresCiviles } from 'src/app/@data/model/entity';
import { ConstantPool } from '@angular/compiler';
import { Const } from 'src/app/@data/services/const';
import { ServidoresCivilesGraficosDonats } from 'src/app/@data/model/servidoresCivilesGraficosDonats';
import { GenericoDTO } from 'src/app/@data/model/graficoGenerico';

export interface ChartOptions {
  series: ApexNonAxisChartSeries;
  tooltip: ApexTooltip;
  chart: ApexChart;
  responsive: ApexResponsive[];
  labels: any;
  fill: any;
  stroke: ApexStroke;
  states: ApexStates;
  legend: ApexLegend;
  title: ApexTitleSubtitle;
  theme: ApexTheme;
  plotOptions: ApexPlotOptions;
  dataLabels: ApexDataLabels;
}

@Component({
  selector: 'gme-web-entidad',
  templateUrl: './entidad.component.html',
  styleUrls: ['./entidad.component.scss'],
})
export class EntidadComponent implements OnInit, AfterContentChecked {
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  entity: Entity;
  isGDR: boolean;

  imgEntidad = './assets/images/logo.png';
  name = 'Angular';
  @ViewChild('chart') chart: ChartComponent;
  public chartOptionsTipoOrgano: Partial<ChartOptions>;
  public chartOptionsServidoresRegimenLaboral: Partial<ChartOptions>;

  constructor(
    private authRepository: AuthenticationRepository,
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private cdr: ChangeDetectorRef
  ) {}

  ngOnInit(): void {
    this.user = this.authRepository.getCurrentUserValue;
    setTimeout(() => {
      this.subscribeEvents();
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    }, 0);
  }

  ngAfterContentChecked(): void {
    this.cdr.detectChanges();
  }

  tipoAplicacion($event) {
    if ($event.index === 0) {
      this.isGDR = false;
    } else {
      this.isGDR = true;
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
}
