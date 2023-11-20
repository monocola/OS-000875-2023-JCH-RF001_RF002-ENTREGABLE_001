import { Component, HostListener, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { NbSidebarService } from '@nebular/theme';
import FileSaver from 'file-saver';
import moment from 'moment';
import { SidenavService } from 'src/app/@data/services/sidenav.service';
import { GraficosRepository } from 'src/app/@domain/repository/graficos.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { base64ToFilePromise } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-servir',
  templateUrl: './servir.component.html',
  styleUrls: ['./servir.component.scss'],
})
export class ServirComponent implements OnInit {
  filterForm: FormGroup;
  rangePickerStatus: string = 'basic';
  barWidth: string = '100%';
  condiciones = [];
  displayedColumnsTable: string[] = ['nombre', 'convocatorias'];
  numGestores: number = 0;
  numBases: number = 0;
  numConvocatorias: number = 0;
  numPerfiles: number = 0;
  numVacantes: number = 0;
  numPostulantes: number = 0;

  basicData: any;
  basicOptions: any;

  dataEstadoPostulante: any;

  optionsEstadoPostulante: any;

  estadoBases = [];

  fechaExcel: string = '';
  condicionExcel: string = '';
  estadoExcel: string = '';

  mayor = [];

  menor = [];

  optionsMayorPerfiles: any;
  dataMayorPerfiles: any;

  dataMenorPerfiles: any;
  optionsMenorPerfiles: any;

  dataPromedio: any;
  optionsPromedio: any;

  TopPerfilPorcentaje: '0 %';
  LowPerfilPorcentaje: '0 %';

  colorEntidades = [
    '#009edc',
    '#9a9a9a',
    '#a64dde',
    '#ffa560',
    '#ffc421',
    '#009edc',
    '#00b167',
    '#ff4a4b',
    '#00b167',
    '#6ACF4F',
    '#EE8F60',
  ];
  listaEntidades = [];
  listaMeses = [];
  tablaEntidades = [];

  listPromedio = [];

  constructor(
    private fb: FormBuilder,
    private maestraService: MaestraRepository,
    public graficoService: GraficosRepository,
    public sidenavService: SidenavService,
    public sidebarService: NbSidebarService,
    private toast: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.cargarCombox();
    this.limpiar();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      condicion: null,
      estado: null,
      fecha: null,
    });
  }

  buscar() {
    if (!this.filtroFecha()) {
      this.toast.showToast('Elegir un rango de fecha correcto.', 'warning');
      return;
    } else {
      let fecIni = moment(this.f.fecha.value.start).format('YYYY-MM-DD');
      let fecFin = moment(this.f.fecha.value.end).format('YYYY-MM-DD');
      this.f.condicion.setValue(
        this.f.condicion.value === '' ? null : this.f.condicion.value
      );
      this.f.estado.setValue(
        this.f.estado.value === '' ? null : this.f.estado.value
      );
      this.datosExcel(
        this.f.fecha.value,
        this.f.condicion.value,
        this.f.estado.value
      );
      this.cabeceraGeneral(fecIni, fecFin);
      this.graficoRankingPerfiles(fecIni, fecFin);
      this.topEntidades(fecIni, fecFin);
      this.regimenByConvocatorias(fecIni, fecFin);
      this.tiempoPromedio(fecIni, fecFin);
    }
  }

  limpiar() {
    this.initializeForm();
    let fecha1: Date = new Date();
    let fecha2: Date = new Date();

    fecha1.setMonth(fecha1.getMonth() - 12);
    let fec = {
      start: fecha1,
      end: fecha2,
    };
    this.f.fecha.setValue(fec);

    let fecIni = moment(this.f.fecha.value.start).format('YYYY-MM-DD');
    let fecFin = moment(this.f.fecha.value.end).format('YYYY-MM-DD');
    this.datosExcel(
      this.f.fecha.value,
      this.f.condicion.value,
      this.f.estado.value
    );
    this.cabeceraGeneral(fecIni, fecFin);
    this.graficoRankingPerfiles(fecIni, fecFin);
    this.topEntidades(fecIni, fecFin);
    this.regimenByConvocatorias(fecIni, fecFin);
    this.tiempoPromedio(fecIni, fecFin);
  }
  cargarCombox() {
    this.maestraService
      .getMaestraDetalleByCod('TIP_COND_PUESTO')
      .subscribe((res) => {
        this.condiciones = res;
      });
  }

  graficoEntidades() {
    this.basicData = {
      labels: this.listaMeses,
      datasets: this.listaEntidades,
    };

    this.basicOptions = {
      legend: {
        display: false,
      },
      plugins: {
        legend: {
          labels: {
            color: '#495057',
          },
        },
      },
      scales: {
        xAxes: [
          {
            ticks: {
              fontColor: 'black',
            },
          },
        ],
        yAxes: [
          {
            ticks: {
              fontColor: 'black',
              beginAtZero: 0,
              min: 0,
            },
          },
        ],
      },
    };
  }

  graficoRankingPerfiles(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoBarrasPerfiles(
        this.f.condicion.value,
        this.f.estado.value,
        fecIni,
        fecFin
      )
      .subscribe((res) => {
        this.TopPerfilPorcentaje = res.porcetajeMayor;
        this.LowPerfilPorcentaje = res.porcetajeMenor;
        this.mayor = res.mayor;
        this.menor = res.menor;

        this.cargarTopPerfiles();
        this.cargarMinorPerfiles();
      });
  }

  cargarTopPerfiles() {
    let descripcion: string[] = [];
    let valor: number[] = [];

    this.mayor.forEach((e) => {
      descripcion.push(e.nombre + ' (' + e.data + ')');
      valor.push(e.data);
    });

    this.dataMayorPerfiles = {
      labels: descripcion,
      datasets: [
        {
          label: '',
          backgroundColor: '#42A5F5',
          data: valor,
        },
      ],
    };

    this.optionsMayorPerfiles = {
      tooltips: {
        enabled: false,
      },
      indexAxis: 'y',
      legend: {
        display: false,
      },
      scales: {
        yAxes: [
          {
            display: true,
            gridLines: {
              display: false,
            },
            scaleLabel: {
              show: true,
              labelString: 'Value',
            },
            ticks: {
              beginAtZero: true,
              max: 100,
              min: 0,
            },
          },
        ],
        xAxes: [
          {
            gridLines: {
              display: false,
            },
            categoryPercentage: 1,
            barPercentage: 1,
            ticks: {
              display: false,
              beginAtZero: 0,
              min: 0,
            },
          },
        ],
      },
    };
  }

  cargarMinorPerfiles() {
    let descripcion: string[] = [];
    let valor: number[] = [];

    this.menor.forEach((e) => {
      descripcion.push(e.nombre + ' (' + e.data + ')');
      valor.push(e.data);
    });
    this.dataMenorPerfiles = {
      labels: descripcion,
      datasets: [
        {
          label: '',
          backgroundColor: '#FFA726',
          data: valor,
        },
      ],
    };

    this.optionsMenorPerfiles = {
      tooltips: {
        enabled: false,
      },
      indexAxis: 'y',
      legend: {
        display: false,
      },
      scales: {
        yAxes: [
          {
            display: true,
            gridLines: {
              display: false,
            },
            scaleLabel: {
              show: true,
              labelString: 'Value',
            },
            ticks: {
              beginAtZero: true,
              max: 100,
              min: 0,
            },
          },
        ],
        xAxes: [
          {
            gridLines: {
              display: false,
            },
            categoryPercentage: 1,
            barPercentage: 1,
            ticks: {
              display: false,
              beginAtZero: 0,
              min: 0,
            },
          },
        ],
      },
    };
  }

  cargarGraficoEstadoBases() {
    let descripcion: string[] = [];
    let valor: number[] = [];
    let suma = 0;

    this.estadoBases.forEach((e) => {
      descripcion.push(e.nombre);
      valor.push(e.data);
      suma += e.data;
    });

    this.dataEstadoPostulante = {
      labels: descripcion,
      datasets: [
        {
          data: valor,
          backgroundColor: [
            '#00c3ff',
            '#78defb',
            '#7a3900',
            '#008abd',
            '#EE8F60',
            '#6ACF4F',
          ],
          hoverBackgroundColor: [
            '#00c3ff',
            '#78defb',
            '#7a3900',
            '#008abd',
            '#EE8F60',
            '#6ACF4F',
          ],
        },
      ],
    };
    this.optionsEstadoPostulante = {
      onHover: (event, chartElement) => {
        event.target.style.cursor = chartElement[0] ? 'pointer' : 'default';
        /* if (!chartElement[0]) {
          this.gestoresBase = [];
        } */
      },
      cutoutPercentage: 50,
      plugins: {
        doughnutlabel: {
          labels: [
            {
              text: suma,
              font: {
                size: '60',
                units: 'em',
                family: 'Arial, Helvetica, sans-serif',
                style: 'italic',
                weight: 'bold',
              },
              color: '#bc2c1a',
            },
            {
              text: 'convocatorias',
              font: {
                size: '30',
              },
              color: 'black',
            },
          ],
        },
        labels: [
          {
            render: 'label',
            fontSize: 11,
            position: 'outside',
            fontColor: 'black',
          },
          {
            render: 'percentage',
            fontColor: 'white',
          },
        ],
      },
      title: {
        display: false,
        text:
          'Cantidad de Postulantes por Seguimiento de procesos de postulación',
        fontSize: 12,
      },
      legend: {
        display: false,
      },
    };
  }

  cabeceraGeneral(fecIni: string, fecFin: string) {
    this.graficoService
      .cabeceraServir(this.f.condicion.value, fecIni, fecFin)
      .subscribe((res) => {
        this.numGestores = res.numGestores;
        this.numBases = res.numBases;
        this.numConvocatorias = res.numConvocatorias;
        this.numPerfiles = res.numPerfiles;
        this.numVacantes = res.numVacantes;
        this.numPostulantes = res.numPostulantes;
      });
  }

  topEntidades(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoLineEntidadesServir(this.f.condicion.value, fecIni, fecFin)
      .subscribe((res) => {
        let nombres = [];
        let m = [];
        let d = [];

        let m1 = [];
        let d1 = [];
        this.tablaEntidades = res;
        this.tablaEntidades.forEach((e) => {
          nombres.push(e.nombreEntidad);
          m = [];
          d = [];
          e.meses.forEach((element) => {
            m.push(element.nombre);
            d.push(element.data);
          });
          m1.push(m);
          d1.push(d);
        });
        this.parsearListaEntidad(nombres, m1, d1);
      });
  }

  parsearListaEntidad(nombres: any[], meses: any[], data: any[]) {
    let lista = [];
    nombres.forEach((e, i) => {
      let obj = {
        label: e,
        data: data[i],
        fill: false,
        borderColor: this.colorEntidades[i],
        tension: 0,
      };
      lista.push(obj);
    });
    this.listaMeses = meses.length !== 0 ? meses[0] : [];
    this.listaEntidades = lista;
    this.graficoEntidades();
  }

  regimenByConvocatorias(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoRegimenServir(this.f.condicion.value, fecIni, fecFin)
      .subscribe((res) => {
        this.estadoBases = res.items;
        this.cargarGraficoEstadoBases();
      });
  }

  tiempoPromedio(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoBarraPromedioConvocatoria(this.f.condicion.value, fecIni, fecFin)
      .subscribe((res) => {
        this.listPromedio = res.items;
        this.cargarGraficoTiempoPromedio();
      });
  }

  cargarGraficoTiempoPromedio() {
    let descripcion: string[] = [];
    let valor: number[] = [];

    this.listPromedio.forEach((e) => {
      descripcion.push(e.nombre);
      valor.push(e.data);
    });
    this.dataPromedio = {
      labels: descripcion,
      datasets: [
        {
          label: '',
          backgroundColor: '#42A5F5',
          data: [
            valor[0] === 0 ? [0, 0] : [0, 3],
            valor[1] === 0 ? [0, 0] : [2, 5],
            valor[2] === 0 ? [0, 0] : [4, 7],
            valor[3] === 0 ? [0, 0] : [6, 9],
          ],
        },
      ],
    };

    this.optionsPromedio = {
      indexAxis: 'y',
      legend: {
        display: false,
      },
      tooltips: {
        enabled: false,
      },
      scales: {
        yAxes: [
          {
            display: true,
            gridLines: {
              display: true,
            },
            scaleLabel: {
              show: true,
              labelString: 'Value',
            },
            ticks: {
              beginAtZero: true,
              max: 100,
              min: 0,
            },
          },
        ],
        xAxes: [
          {
            gridLines: {
              display: false,
            },
            categoryPercentage: 1,
            barPercentage: 1,
            ticks: {
              display: false,
              beginAtZero: 0,
              max: 10,
              min: 0,
            },
          },
        ],
      },
    };
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  datosExcel(fecha: any, condicion: string, estado: string) {
    // this.gestoresBase = [];
    this.fechaExcel =
      moment(fecha.start).format('DD-MM-YYYY') +
      ' al ' +
      moment(fecha.end).format('DD-MM-YYYY');
    this.condicionExcel =
      condicion === null || condicion === ''
        ? 'TODOS'
        : this.condiciones.find((item) => condicion === item.codProg)
            .descripcion;

    /* this.estadoExcel =
      estado === null || estado === ''
        ? 'TODOS'
        : this.estados.find((item) => estado === item.codProg).descripcion;*/
  }
  filtroFecha(): boolean {
    if (
      this.f.fecha.value.start === undefined ||
      this.f.fecha.value.end === undefined
    ) {
      return false;
    } else {
      return true;
    }
  }

  @HostListener('window:beforeprint', ['$event'])
  onBeforePrint() {
    this.barWidth = '65%';
    this.sidebarService.compact();
  }

  @HostListener('window:afterprint', ['$event'])
  onAfterPrint() {
    this.sidebarService.expand();
    this.barWidth = '100%';
  }

  print() {
    let excel = {
      fecha: this.fechaExcel,
      condicion: this.condicionExcel,
      estado: this.estadoExcel,
      entidades: this.tablaEntidades,
      regimen: this.estadoBases,
      topPerfiles: this.mayor,
      lowPerfiles: this.menor,
    };

    this.graficoService.downloadExcelServir(excel).subscribe(
      (res) => {
        const nameFile = `CuadroDeMandoServir.xlsx`;
        const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toast.showToast('Ocurrió un error.', 'danger')
    );
  }
}
