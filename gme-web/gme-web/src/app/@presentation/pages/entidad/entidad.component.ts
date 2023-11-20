import {
  Component,
  OnInit,
  Inject,
  NgZone,
  PLATFORM_ID,
  ViewChild,
} from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { User } from 'src/app/@data/model/user';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from '../../../@data/services/sidenav.service';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { MatDialog } from '@angular/material/dialog';
import { ModalEditarComponent } from './modal-editar/modal-editar.component';
// amCharts imports
import { ChartComponent } from "ng-apexcharts";
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
  ApexTitleSubtitle
} from "ng-apexcharts";
import { EntidadRepository } from 'src/app/@domain/repository/entidad.repository';
import { forkJoin } from 'rxjs';
import { EntidadService } from 'src/app/@data/services/entidad.service';
import { Entity, ResumenServidoresCiviles } from 'src/app/@data/model/entity';
import { ConstantPool } from '@angular/compiler';
import { Const } from 'src/app/@data/services/const';
import { ServidoresCivilesGraficosDonats } from 'src/app/@data/model/servidoresCivilesGraficosDonats';
import { GenericoDTO } from 'src/app/@data/model/graficoGenerico';

export interface  ChartOptions {
  series: ApexNonAxisChartSeries;
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
export class EntidadComponent implements OnInit {
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  entity: Entity;
  resumenServidoresCiviles: ResumenServidoresCiviles;
  servidoresCivilesTipoOrgano: GenericoDTO[];
  servidoresCivilesRegimenLaboral: GenericoDTO[];
  servidoresCivilesRegimenLaboralLabel: GenericoDTO[];

  arrayServidoresCivilesTipoOrgano: any[] = [];
  arrayServidoresCivilesTipoOrganoLabel: any[] = [];

  arrayServidoresCivilesRegimenLaboral: any[] = [];
  arrayServidoresCivilesRegimenLaboralLabel: any[] = [];

  imgEntidad = './assets/images/logo-servir.png';
  name = 'Angular';
  @ViewChild('chart') chart: ChartComponent;
  public chartOptionsTipoOrgano: Partial<ChartOptions>;
  public chartOptionsServidoresRegimenLaboral: Partial<ChartOptions>;


  constructor(
    private authRepository: AuthenticationRepository,
    private entidadService: EntidadService,
    private authenticationService: AuthenticationRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private dialog: MatDialog,
    private zone: NgZone
  ) {}

  ngOnInit(): void {
    this.user = this.authRepository.getCurrentUserValue;
    setTimeout(() => {
      this.subscribeEvents();
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
    }, 0);

    this.listarResumenes();
    this.listEntidad();
  }

  procesarDataGraficos(result: ServidoresCivilesGraficosDonats) {
    this.servidoresCivilesTipoOrgano = result.servidoresCivilesTipoOrgano;
    this.servidoresCivilesRegimenLaboral =
      result.servidoresCivilesRegimenLaboral;

    for (let i = 0; i < this.servidoresCivilesTipoOrgano.length; i++) {
      this.arrayServidoresCivilesTipoOrgano.push(
        Number(this.servidoresCivilesTipoOrgano[i].descripcion)
      );
      this.arrayServidoresCivilesTipoOrganoLabel.push(
        this.servidoresCivilesTipoOrgano[i].codigoTexto
      );
    }

    if (this.servidoresCivilesTipoOrgano.length === 0) {
      this.arrayServidoresCivilesTipoOrgano.push( Number(0));
      this.arrayServidoresCivilesTipoOrganoLabel.push('no hay información');
    }

    for (let i = 0; i < this.servidoresCivilesRegimenLaboral.length; i++) {
      this.arrayServidoresCivilesRegimenLaboral.push(
        Number(this.servidoresCivilesRegimenLaboral[i].descripcion)
      );
      this.arrayServidoresCivilesRegimenLaboralLabel.push(
        this.servidoresCivilesRegimenLaboral[i].codigoTexto
      );
    }
    if (this.servidoresCivilesRegimenLaboral.length === 0) {
      this.arrayServidoresCivilesRegimenLaboral.push(Number(0));
      this.arrayServidoresCivilesRegimenLaboralLabel.push('no hay información');
    }

    this.listDonatsServidoresCiviles();
  }

  listDonatsServidoresCiviles() {
    // this.chartOptionsTipoOrgano = {
    //   series: this.arrayServidoresCivilesTipoOrgano,
    //   chart: {
    //     type: 'donut',
    //   },
    //   labels: this.arrayServidoresCivilesTipoOrganoLabel,
    //   responsive: [
    //     {
    //       breakpoint: 480,
    //       options: {
    //         chart: {
    //           width: 200,
    //         },
    //         legend: {
    //           position: 'bottom',
    //         },
    //       },
    //     },
    //   ],
    // };

    this.chartOptionsTipoOrgano = {
      series: this.arrayServidoresCivilesTipoOrgano,
      chart: {
        // width: 380,
        type: "donut",
        // dropShadow: {
        //   enabled: true,
        //   color: "#111",
        //   top: -1,
        //   left: 3,
        //   blur: 3,
        //   opacity: 0.2
        // }
      },
      stroke: {
        width: 0
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              total: {
                showAlways: true,
                show: true
              }
            }
          }
        }
      },
      labels: this.arrayServidoresCivilesTipoOrganoLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8
        }
      },
      fill: {
        type: "pattern",
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            "verticalLines",
            "squares",
            "horizontalLines",
            "circles",
            "slantedLines"
          ]
        }
      },
      states: {
        hover: {
          filter: {
            type: "none"
          }
        }
      },
      theme: {
        palette: "palette2"
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200
            },
            legend: {
              position: "bottom"
            }
          }
        }
      ]
    };


    this.chartOptionsServidoresRegimenLaboral = {
      series: this.arrayServidoresCivilesRegimenLaboral,
      chart: {
        // width: 380,
        type: "donut",
        // dropShadow: {
        //   enabled: true,
        //   color: "#111",
        //   top: -1,
        //   left: 3,
        //   blur: 3,
        //   opacity: 0.2
        // }
      },
      stroke: {
        width: 0
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              total: {
                showAlways: true,
                show: true
              }
            }
          }
        }
      },
      labels: this.arrayServidoresCivilesRegimenLaboralLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8
        }
      },
      fill: {
        type: "pattern",
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            "verticalLines",
            "squares",
            "horizontalLines",
            "circles",
            "slantedLines"
          ]
        }
      },
      states: {
        hover: {
          filter: {
            type: "none"
          }
        }
      },
      theme: {
        palette: "palette2"
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200
            },
            legend: {
              position: "bottom"
            }
          }
        }
      ]
    };


  }

  listEntidad() {
    //  const entidad =  this.entidadService.getListarEntidad();
    this.entity = JSON.parse(sessionStorage.getItem('entidad'));
    console.log('entidad :', this.entity);
    this.imgEntidad = null;
    if (this.entity !== null && this.entity.logo !== null) {
      this.imgEntidad = Const.API_FILE_SERVER + this.entity.logo;
    }
  }

  listarResumenes() {
    const getListaResumensServidoresCiviles =
      this.entidadService.getListaResumensServidoresCiviles();
    const getListaResumensServidoresCivilesDonats =
      this.entidadService.getListaResumensServidoresCivilesGraficosDonats();
    forkJoin([
      getListaResumensServidoresCiviles,
      getListaResumensServidoresCivilesDonats,
    ]).subscribe((results) => {
      console.log(results[0][0]);
      this.resumenServidoresCiviles = results[0][0];
      console.log('resumenes', this.resumenServidoresCiviles);
      this.procesarDataGraficos(results[1]);
      console.log('donats:', results[1]);
      //  this.listGestores = results[1];
    });
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
        this.ngOnInit();
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
