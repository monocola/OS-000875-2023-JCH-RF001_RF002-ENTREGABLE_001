import {
  Component,
  OnInit,
  Inject,
  NgZone,
  PLATFORM_ID,
  ViewChild,
} from '@angular/core';
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
import {
  Entity,
  ResumenServidoresCivilesGDR,
} from 'src/app/@data/model/entity';
import { GenericoDTO } from 'src/app/@data/model/graficoGenerico';
import { forkJoin } from 'rxjs';
import { Const } from 'src/app/@data/services/const';
import { ServidoresCivilesGDRGraficosDonats } from 'src/app/@data/model/servidoresCivilesGDRGraficosDonats';

@Component({
  selector: 'gdr-web-gdr',
  templateUrl: './gdr.component.html',
  styleUrls: ['./gdr.component.scss'],
})
export class GdrComponent implements OnInit {
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  user: User;
  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  entity: Entity;
  resumenServidoresCiviles: ResumenServidoresCivilesGDR;
  servidoresCivilesTipoOrgano: GenericoDTO[];

  servidoresCivilesRegimenLaboral: GenericoDTO[];
  servidoresCivilesRegimenLaboralLabel: GenericoDTO[];

  servidoresCivilesCarrerasEspeciales: GenericoDTO[];
  servidoresCivilesCarrerasEspecialesLabel: GenericoDTO[];

  servidoresCivilesPorSegmento: GenericoDTO[];
  servidoresCivilesPorSegmentoLabel: GenericoDTO[];

  servidoresCivilesSindicalizados: GenericoDTO[];
  servidoresCivilesSindicalizadosLabel: GenericoDTO[];

  arrayServidoresCivilesTipoOrgano: any[] = [];
  arrayServidoresCivilesTipoOrganoLabel: any[] = [];

  arrayServidoresCivilesRegimenLaboral: any[] = [];
  arrayServidoresCivilesRegimenLaboralLabel: any[] = [];

  arrayServidoresCivilesSindicalizados: any[] = [];
  arrayServidoresCivilesSindicalizadosLabel: any[] = [];

  arrayServidoresCivilesPorSegmento: any[] = [];
  arrayServidoresCivilesPorSegmentoLabel: any[] = [];

  arrayServidoresCivilesCarrerasEspeciales: any[] = [];
  arrayServidoresCivilesCarrerasEspecialesLabel: any[] = [];

  imgEntidad = './assets/images/logo.png';
  name = 'Angular';
  @ViewChild('chart') chart: ChartComponent;
  public chartOptionsTipoOrgano: Partial<ChartOptions>;
  public chartOptionsServidoresRegimenLaboral: Partial<ChartOptions>;
  public chartOptionsCarrerasEspeciales: Partial<ChartOptions>;
  public chartOptionsPorSegmento: Partial<ChartOptions>;
  public chartOptionsSindicalizados: Partial<ChartOptions>;

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

  procesarDataGraficos(result: ServidoresCivilesGDRGraficosDonats) {
    this.servidoresCivilesTipoOrgano = result.servidoresCivilesTipoOrganoGdr;
    this.servidoresCivilesRegimenLaboral =
      result.servidoresCivilesRegimenLaboralGdr;
    this.servidoresCivilesCarrerasEspeciales =
      result.servidoresCivilesCarrerasEspecialesGdr;
    this.servidoresCivilesPorSegmento = result.servidoresCivilesPorSegmentoGdr;
    this.servidoresCivilesSindicalizados =
      result.servidoresCivilesSindicalizadosGdr;

    for (let i = 0; i < this.servidoresCivilesTipoOrgano.length; i++) {
      this.arrayServidoresCivilesTipoOrgano.push(
        Number(this.servidoresCivilesTipoOrgano[i].descripcion)
      );
      this.arrayServidoresCivilesTipoOrganoLabel.push(
        this.servidoresCivilesTipoOrgano[i].codigoTexto
      );
    }

    if (this.servidoresCivilesTipoOrgano.length === 0) {
      this.arrayServidoresCivilesTipoOrgano.push(Number(0));
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

    for (let i = 0; i < this.servidoresCivilesCarrerasEspeciales.length; i++) {
      this.arrayServidoresCivilesCarrerasEspeciales.push(
        Number(this.servidoresCivilesCarrerasEspeciales[i].descripcion)
      );
      this.arrayServidoresCivilesCarrerasEspecialesLabel.push(
        this.servidoresCivilesCarrerasEspeciales[i].codigoTexto
      );
    }
    if (this.servidoresCivilesCarrerasEspeciales.length === 0) {
      this.arrayServidoresCivilesCarrerasEspeciales.push(Number(0));
      this.arrayServidoresCivilesCarrerasEspecialesLabel.push(
        'no hay información'
      );
    }

    for (let i = 0; i < this.servidoresCivilesPorSegmento.length; i++) {
      this.arrayServidoresCivilesPorSegmento.push(
        Number(this.servidoresCivilesPorSegmento[i].descripcion)
      );
      this.arrayServidoresCivilesPorSegmentoLabel.push(
        this.servidoresCivilesPorSegmento[i].codigoTexto
      );
    }
    if (this.servidoresCivilesPorSegmento.length === 0) {
      this.arrayServidoresCivilesPorSegmento.push(Number(0));
      this.arrayServidoresCivilesPorSegmentoLabel.push('no hay información');
    }

    for (let i = 0; i < this.servidoresCivilesSindicalizados.length; i++) {
      this.arrayServidoresCivilesSindicalizados.push(
        Number(this.servidoresCivilesSindicalizados[i].descripcion)
      );
      this.arrayServidoresCivilesSindicalizadosLabel.push(
        this.servidoresCivilesSindicalizados[i].codigoTexto
      );
    }

    if (this.servidoresCivilesSindicalizados.length === 0) {
      this.arrayServidoresCivilesSindicalizados.push(Number(0));
      this.arrayServidoresCivilesSindicalizadosLabel.push('no hay información');
    }
    this.listDonatsServidoresCiviles();
  }

  listDonatsServidoresCiviles() {
    this.chartOptionsTipoOrgano = {
      series: this.arrayServidoresCivilesTipoOrgano,
      tooltip: {
        custom: function(opts) {
          const data =
            opts.ctx.w.config.series[opts.seriesIndex]
            const color =
            opts.ctx.w.globals.colors[opts.seriesIndex]
      
            return '<div style="width:120px;height:50px; background-color:'+color+'; border-color:black;position:relative;"><span style="position:absolute;top:50%;left:50%;transform: translate(-50%, -50%);">' 
            + data +'</span>' +
            '</div>';
        }
      },
      chart: {
        type: 'donut',
      },
      stroke: {
        width: 0,
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              name: {
                show: true,
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600,
                color: undefined,
                offsetY: 30,
                formatter: function (val) {
                  return val
                }
              },
              
              total: {
                show: true,
                showAlways: true,
                label: 'Total',
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600, 
                color: '#8191A4',
                
                formatter: function (w) {
                  return w.globals.seriesTotals.reduce((a, b) => {
                    return a + b
                  }, 0)
                }
              },
              value: {
                show: true,
                fontSize: '26px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 400,
                color: '#222b45',
                offsetY: -25,
                formatter: function (val) {
                  return val
                }
              }
          }}
        }
      },
      labels: this.arrayServidoresCivilesTipoOrganoLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8,
        },
      },
      fill: {
        type: 'pattern',
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            'verticalLines',
            'squares',
            'horizontalLines',
            'circles',
            'slantedLines',
          ],
        },
      },
      states: {
        hover: {
          filter: {
            type: 'none',
          },
        },
      },
      theme: {
        palette: 'palette2',
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: 'bottom',
            },
          },
        },
      ],
    };

    this.chartOptionsServidoresRegimenLaboral = {
      series: this.arrayServidoresCivilesRegimenLaboral,
      tooltip: {
        custom: function(opts) {
          const data =
            opts.ctx.w.config.series[opts.seriesIndex]
            const color =
            opts.ctx.w.globals.colors[opts.seriesIndex]
      
            return '<div style="width:120px;height:50px; background-color:'+color+'; border-color:black;position:relative;"><span style="position:absolute;top:50%;left:50%;transform: translate(-50%, -50%);">' 
            + data +'</span>' +
            '</div>';
        }
      },
      chart: {
        type: 'donut',
      },
      stroke: {
        width: 0,
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              name: {
                show: true,
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600,
                color: undefined,
                offsetY: 30,
                formatter: function (val) {
                  return val
                }
              },
              
              total: {
                show: true,
                showAlways: true,
                label: 'Total',
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600, 
                color: '#8191A4',
                
                formatter: function (w) {
                  return w.globals.seriesTotals.reduce((a, b) => {
                    return a + b
                  }, 0)
                }
              },
              value: {
                show: true,
                fontSize: '26px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 400,
                color: '#222b45',
                offsetY: -25,
                formatter: function (val) {
                  return val
                }
              }
          }}
        }
      },
      labels: this.arrayServidoresCivilesRegimenLaboralLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8,
        },
      },
      fill: {
        type: 'pattern',
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            'verticalLines',
            'squares',
            'horizontalLines',
            'circles',
            'slantedLines',
          ],
        },
      },
      states: {
        hover: {
          filter: {
            type: 'none',
          },
        },
      },
      theme: {
        palette: 'palette2',
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: 'bottom',
            },
          },
        },
      ],
    };

    this.chartOptionsCarrerasEspeciales = {
      series: this.arrayServidoresCivilesCarrerasEspeciales,
      tooltip: {
        custom: function(opts) {
          const data =
            opts.ctx.w.config.series[opts.seriesIndex]
            const color =
            opts.ctx.w.globals.colors[opts.seriesIndex]
      
            return '<div style="width:120px;height:50px; background-color:'+color+'; border-color:black;position:relative;"><span style="position:absolute;top:50%;left:50%;transform: translate(-50%, -50%);">' 
            + data +'</span>' +
            '</div>';
        }
      },
      chart: {
        type: 'donut',
      },
      stroke: {
        width: 0,
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              name: {
                show: true,
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600,
                color: undefined,
                offsetY: 30,
                formatter: function (val) {
                  return val
                }
              },
              
              total: {
                show: true,
                showAlways: true,
                label: 'Total',
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600, 
                color: '#8191A4',
                
                formatter: function (w) {
                  return w.globals.seriesTotals.reduce((a, b) => {
                    return a + b
                  }, 0)
                }
              },
              value: {
                show: true,
                fontSize: '26px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 400,
                color: '#222b45',
                offsetY: -25,
                formatter: function (val) {
                  return val
                }
              }
          }}
        }
      },
      labels: this.arrayServidoresCivilesCarrerasEspecialesLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8,
        },
      },
      fill: {
        type: 'pattern',
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            'verticalLines',
            'squares',
            'horizontalLines',
            'circles',
            'slantedLines',
          ],
        },
      },
      states: {
        hover: {
          filter: {
            type: 'none',
          },
        },
      },
      theme: {
        palette: 'palette2',
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: 'bottom',
            },
          },
        },
      ],
    };

    this.chartOptionsPorSegmento = {
      series: this.arrayServidoresCivilesPorSegmento,
      tooltip: {
        custom: function(opts) {
          const data =
            opts.ctx.w.config.series[opts.seriesIndex]
            const color =
            opts.ctx.w.globals.colors[opts.seriesIndex]
      
            return '<div style="width:120px;height:50px; background-color:'+color+'; border-color:black;position:relative;"><span style="position:absolute;top:50%;left:50%;transform: translate(-50%, -50%);">' 
            + data +'</span>' +
            '</div>';
        }
      },
      chart: {
        type: 'donut',
      },
      stroke: {
        width: 0,
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              name: {
                show: true,
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600,
                color: undefined,
                offsetY: 30,
                formatter: function (val) {
                  return val
                }
              },
              
              total: {
                show: true,
                showAlways: true,
                label: 'Total',
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600, 
                color: '#8191A4',
                
                formatter: function (w) {
                  return w.globals.seriesTotals.reduce((a, b) => {
                    return a + b
                  }, 0)
                }
              },
              value: {
                show: true,
                fontSize: '26px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 400,
                color: '#222b45',
                offsetY: -25,
                formatter: function (val) {
                  return val
                }
              }
          }}
        }
      },
      labels: this.arrayServidoresCivilesPorSegmentoLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8,
        },
      },
      fill: {
        type: 'pattern',
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            'verticalLines',
            'squares',
            'horizontalLines',
            'circles',
            'slantedLines',
          ],
        },
      },
      states: {
        hover: {
          filter: {
            type: 'none',
          },
        },
      },
      theme: {
        palette: 'palette2',
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: 'bottom',
            },
          },
        },
      ],
    };

    this.chartOptionsSindicalizados = {
      series: this.arrayServidoresCivilesSindicalizados,
      tooltip: {
        custom: function(opts) {
          const data =
            opts.ctx.w.config.series[opts.seriesIndex]
            const color =
            opts.ctx.w.globals.colors[opts.seriesIndex]
      
            return '<div style="width:120px;height:50px; background-color:'+color+'; border-color:black;position:relative;"><span style="position:absolute;top:50%;left:50%;transform: translate(-50%, -50%);">' 
            + data +'</span>' +
            '</div>';
        }
      },
      chart: {
        type: 'donut',
      },
      stroke: {
        width: 0,
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: true,
              name: {
                show: true,
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600,
                color: undefined,
                offsetY: 30,
                formatter: function (val) {
                  return val
                }
              },
              
              total: {
                show: true,
                showAlways: true,
                label: 'Total',
                fontSize: '22px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 600, 
                color: '#8191A4',
                
                formatter: function (w) {
                  return w.globals.seriesTotals.reduce((a, b) => {
                    return a + b
                  }, 0)
                }
              },
              value: {
                show: true,
                fontSize: '26px',
                fontFamily: 'Helvetica, Arial, sans-serif',
                fontWeight: 400,
                color: '#222b45',
                offsetY: -25,
                formatter: function (val) {
                  return val
                }
              }
          }}
        }
      },
      labels: this.arrayServidoresCivilesSindicalizadosLabel,
      dataLabels: {
        dropShadow: {
          blur: 3,
          opacity: 0.8,
        },
      },
      fill: {
        type: 'pattern',
        opacity: 1,
        pattern: {
          enabled: true,
          style: [
            'verticalLines',
            'squares',
            'horizontalLines',
            'circles',
            'slantedLines',
          ],
        },
      },
      states: {
        hover: {
          filter: {
            type: 'none',
          },
        },
      },
      theme: {
        palette: 'palette2',
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: 'bottom',
            },
          },
        },
      ],
    };
  }

  listEntidad() {
    //  const entidad =  this.entidadService.getListarEntidad();
    this.entity = JSON.parse(sessionStorage.getItem('entidad'));
    console.log('entidad :', this.entity);
    if (this.entity !== null && this.entity.logo !== null) {
      this.imgEntidad = Const.API_FILE_SERVER + this.entity.logo;
    }
  }

  listarResumenes() {
    const getListaResumensServidoresCiviles =
      this.entidadService.getListaResumensServidoresCivilesGDR();
    const getListaResumensServidoresCivilesDonats =
      this.entidadService.getListaResumensServidoresCivilesGDRGraficosDonats();
    forkJoin([
      getListaResumensServidoresCiviles,
      getListaResumensServidoresCivilesDonats,
    ]).subscribe((results) => {
      console.log("DATAAAA", results)
      // console.log("resumenesssss:",results);
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
