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
  selector: 'serv-talento-admin',
  templateUrl: './admin.component.html',
  styleUrls: ['./admin.component.scss'],
})
export class AdminComponent implements OnInit {
  filterForm: FormGroup;
  rangePickerStatus: string = 'basic';
  barWidth: string = '100%';
  displayedColumnsTable: string[] = ['nombre'];
  displayedColumnsTableConvocatorias: string[] = [
    'usuario',
    'vigente',
    'finalizada',
    'cancelada',
    'desierta',
    'total',
  ];

  backColor: string[] = [];
  condiciones = [];

  numGestores: number = 0;
  numBases: number = 0;
  numConvocatorias: number = 0;
  numPerfiles: number = 0;
  numVacantes: number = 0;
  numPostulantes: number = 0;

  trabajos = [];
  regimen = [];
  tipoPracticas = [];

  stackedData: any;
  stackedOptions: any;

  data: any;
  options: any;

  dataPracticantes: any;
  optionsPracticantes: any;

  admin: boolean = true;
  roles = [];

  gestores = [];

  dataGestores: any;
  optionsGestores: any;

  dataMenorPerfiles: any;
  optionsMenorPerfiles: any;

  menor = [];

  usuariosConvocatorias = [];

  dataPromedio: any;
  optionsPromedio: any;

  listPromedio = [];

  fechaExcel: string = '';
  condicionExcel: string = '';
  entidadId = JSON.parse(sessionStorage.getItem('persona')).entidadId;

  constructor(
    private fb: FormBuilder,
    public sidenavService: SidenavService,
    public sidebarService: NbSidebarService,
    public graficoService: GraficosRepository,
    private maestraService: MaestraRepository,
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
      fecha: null,
    });
  }

  cargarCombox() {
    this.maestraService
      .getMaestraDetalleByCod('TIP_COND_PUESTO')
      .subscribe((res) => {
        this.condiciones = res;
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

      this.datosExcel(this.f.fecha.value, this.f.condicion.value);
      this.cabeceraGeneral(fecIni, fecFin);
      this.graficosPuestosTotales(fecIni, fecFin);
      this.graficosRegimenPractica(fecIni, fecFin);
      // this.graficoUsuarios(fecIni, fecFin);
      this.graficoEtapaDifusion(fecIni, fecFin);
      this.tablaUserConvocatorias(fecIni, fecFin);
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
    this.datosExcel(this.f.fecha.value, this.f.condicion.value);
    this.cabeceraGeneral(fecIni, fecFin);
    this.graficosPuestosTotales(fecIni, fecFin);
    this.graficosRegimenPractica(fecIni, fecFin);
    // this.graficoUsuarios(fecIni, fecFin);
    this.graficoEtapaDifusion(fecIni, fecFin);
    this.tablaUserConvocatorias(fecIni, fecFin);
    this.tiempoPromedio(fecIni, fecFin);
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

  cargarGraficoTrabajos() {
    let meses: string[] = [];
    let empleosM: number[] = [];
    let empleosF: number[] = [];
    let practicasM: number[] = [];
    let practicasF: number[] = [];

    this.trabajos.forEach((e) => {
      meses.push(e.nombreMes);
      empleosM.push(e.dataEmpleosHombres);
      empleosF.push(e.dataEmpleosMujeres);
      practicasM.push(e.dataPracticasHombres);
      practicasF.push(e.dataPracticasMujeres);
    });

    this.stackedData = {
      labels: meses,
      datasets: [
        {
          type: 'bar',
          stack: 'Empleos',
          label: 'Empleos M',
          backgroundColor: '#145f7f',
          data: empleosM,
          sex: 'M',
        },
        {
          type: 'bar',
          stack: 'Empleos',
          label: 'Empleos F',
          backgroundColor: '#0c88bc',
          data: empleosF,
          sex: 'F',
        },
        {
          type: 'bar',
          stack: 'Practicas',
          label: 'Practicas M',
          backgroundColor: '#be6b0d',
          data: practicasM,
          sex: 'M',
        },
        {
          type: 'bar',
          stack: 'Practicas',
          label: 'Prácticas F',
          backgroundColor: '#c58d50',
          data: practicasF,
          sex: 'F',
        },
      ],
    };

    this.stackedOptions = {
      plugins: {
        labels: {
          fontSize: 15,
          fontStyle: 'bold',
          fontColor: '#000',
          render: function (args) {
            let valor = '';
            if (args.value !== 0) {
              valor = args.value + (args.dataset.sex === 'M' ? '♂' : '♀');
            }
            return valor;
          },
        },
      },
      legend: {
        onClick: function (e) {
          e.stopPropagation();
        },
        labels: {
          fontColor: '#000080',
          usePointStyle: true,
          generateLabels: function (chart) {
            let labels = chart.data.datasets;

            let legend = labels.map(function (label) {
              let total = 0;
              label.data.forEach((element) => {
                total = total + element;
              });
              return {
                datasetIndex: 0,
                fillStyle: label.backgroundColor,
                text: label.label + ' : ' + total,
              };
            });
            return legend;
          },
        },
      },
      tooltips: {
        mode: 'index',
        intersect: false,
      },
      responsive: true,
      scales: {
        xAxes: [
          {
            stacked: true,
            ticks: {
              fontColor: 'black',
            },
          },
        ],
        yAxes: [
          {
            stacked: true,
            ticks: {
              fontColor: 'black',
              min: 0,
              userCallback: function (label, index, labels) {
                // when the floored value is the same as the value we have a whole number
                if (Math.floor(label) === label) {
                  return label;
                }
              },
            },
          },
        ],
      },
    };
  }

  cargarEmpleos() {
    let descripcion: string[] = [];
    let valor: number[] = [];
    let suma = 0;

    this.regimen.forEach((e) => {
      descripcion.push(e.descripcion);
      valor.push(e.cantidad);
      suma += e.cantidad;
    });

    this.data = {
      labels: descripcion,
      datasets: [
        {
          data: valor,
          backgroundColor: [
            '#005171',
            '#0076a2',
            '#0099d2',
            '#00bef6',
            '#78defb',
          ],
          hoverBackgroundColor: [
            '#005171',
            '#0076a2',
            '#0099d2',
            '#00bef6',
            '#78defb',
          ],
        },
      ],
    };

    this.options = {
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
              text: 'empleos',
              font: {
                size: '50',
              },
              color: 'black',
            },
          ],
        },
        labels: {
          render: 'percentage',
          fontColor: ['white', 'white', 'white', 'white', 'white'],
          precision: 0,
        },
        /*labels: {
          align: 'end',
          anchor: 'end',
          borderRadius: 4,
          backgroundColor: 'teal',
          color: 'white',
          font: {
            weight: 'bold',
          },
        },*/
      },
      title: {
        display: true,
        text: 'Empleos por Régimen',
        fontSize: 16,
        fontColor: 'black',
      },
      legend: {
        display: suma !== 0 ? true : false,
        position: 'right',
        labels: {
          fontColor: '#000080',
          usePointStyle: true,
          generateLabels: function (chart) {
            let labels = chart.data.labels;
            let dataset = chart.data.datasets[0];

            let legend = labels.map(function (label, index) {
              return {
                datasetIndex: 0,
                fillStyle:
                  dataset.backgroundColor && dataset.backgroundColor[index],
                strokeStyle: dataset.borderColor && dataset.borderColor[index],
                lineWidth: dataset.borderWidth,
                text: label + ' : ' + dataset.data[index],
              };
            });
            return legend;
          },
        },
      },
    };
  }

  cargarGraficoPracticantes() {
    let descripcion: string[] = [];
    let valor: number[] = [];
    let suma = 0;

    this.tipoPracticas.forEach((e) => {
      descripcion.push(e.descripcion);
      valor.push(e.cantidad);
      suma += e.cantidad;
    });

    this.dataPracticantes = {
      labels: descripcion,
      datasets: [
        {
          data: valor,
          backgroundColor: ['#cc6613', '#ffa560'],
          hoverBackgroundColor: ['#cc6613', '#ffa560'],
        },
      ],
    };
    this.optionsPracticantes = {
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
              text: 'prácticas',
              font: {
                size: '50',
              },
              color: 'black',
            },
          ],
        },
        labels: {
          render: 'percentage',
          fontColor: ['white', 'white'],
          precision: 0,
        },
      },
      title: {
        display: true,
        text: 'Prácticantes',
        fontSize: 16,
        fontColor: 'black',
      },
      legend: {
        display: suma !== 0 ? true : false,
        position: 'right',
        labels: {
          fontColor: '#000080',
          usePointStyle: true,
          generateLabels: function (chart) {
            let labels = chart.data.labels;
            let dataset = chart.data.datasets[0];

            let legend = labels.map(function (label, index) {
              return {
                datasetIndex: 0,
                fillStyle:
                  dataset.backgroundColor && dataset.backgroundColor[index],
                strokeStyle: dataset.borderColor && dataset.borderColor[index],
                lineWidth: dataset.borderWidth,
                text: label + ' : ' + dataset.data[index],
              };
            });
            return legend;
          },
        },
      },
    };
  }

  cargarGraficoGestores() {
    let descripcion: string[] = [];
    let valor: number[] = [];
    let suma = 0;

    this.backColor = [];
    let admin: boolean = this.admin;
    if (this.gestores.length !== 0) {
      this.roles = [
        {
          nombre: 'Coordinador',
          data: this.gestores.filter((e) => e.rol === 'COORDINADOR').length,
        },
        {
          nombre: 'Gestor',
          data: this.gestores.filter((e) => e.rol === 'GESTOR').length,
        },
      ];
    }

    this.roles.forEach((e) => {
      descripcion.push(e.nombre);
      valor.push(e.data);
      this.backColor.push(
        '#' + (0x1000000 + Math.random() * 0xffffff).toString(16).substr(1, 6)
      );
      suma += e.data;
    });

    let backColor: string[] = this.backColor;

    this.dataGestores = {
      labels: descripcion,
      datasets: [
        {
          data: valor,
          backgroundColor: ['#00c3ff', '#cd6600'],
          hoverBackgroundColor: ['#00c3ff', '#cd6600'],
        },
      ],
    };
    this.optionsGestores = {
      cutoutPercentage: 30,
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
              text: 'usuarios',
              font: {
                size: '50',
              },
              color: 'black',
            },
          ],
        },
        labels: [
          {
            render: function (args) {
              // args will be something like:
              // { label: 'Label', value: 123, percentage: 50, index: 0, dataset: {...} }

              return args.label.split(' ').join('\n');
            },

            fontColor: ['black', 'black', 'black', 'black'],
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

  cargarMinorPerfiles() {

    let descripcion: string[] = [];
    let valor: number[] = [];
    let suma = 0;

    this.menor.forEach((e) => {
      descripcion.push(e.nombre);
      valor.push(e.data);
      suma += e.data;
    });
    let max: number = valor.length !== 0 ? Math.max(...valor) * 2 : 0;
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
      plugins: {
        labels: [
          {
            fontSize: 15,
            fontStyle: 'bold',
            fontColor: '#000',
            render: function (args) {
              return ((args.value / suma) * 100).toFixed(2) + ' %';
            },
          },
          {
            fontSize: 15,
            fontStyle: 'bold',
            fontColor: '#000',
            render: 'value',
            textMargin: -15,
          },
        ],
      },
      legend: {
        display: false,
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
            },
            ticks: {
              // beginAtZero: true,
              max: max,
              min: 0,
            },
          },
        ],
        xAxes: [
          {
            gridLines: {
              display: true,
            },
            // categoryPercentage: 1,
            // barPercentage: 1,
            ticks: {
              display: true,
              // beginAtZero: 0,
              min: 0,
            },
          },
        ],
      },
    };
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

  graficosPuestosTotales(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoBarrasPuestos(
        'coord',
        this.f.condicion.value,
        null,
        fecIni,
        fecFin
      )
      .subscribe((res) => {
        this.trabajos = res.respPuestosTotalesDetalles;
        this.cargarGraficoTrabajos();
      });
  }

  graficosRegimenPractica(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoDonutPuestos(
        'Coordinador',
        this.f.condicion.value,
        null,
        fecIni,
        fecFin
      )
      .subscribe((res) => {
        this.regimen = res.empleo;
        this.tipoPracticas = res.practica;
        this.cargarEmpleos();
        this.cargarGraficoPracticantes();
      });
  }

  graficoUsuarios(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoUsuariosByEntidad(this.f.condicion.value, fecIni, fecFin)
      .subscribe((res) => {
        this.gestores = res.items;
        this.cargarGraficoGestores();
      });
  }

  graficoEtapaDifusion(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoUsuariosByConvDifusion(this.f.condicion.value, fecIni, fecFin, this.entidadId)
      .subscribe((res) => {
        this.menor = res.items;
        this.cargarMinorPerfiles();
      });
  }

  tablaUserConvocatorias(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoUsuariosByConvEstados(this.f.condicion.value, fecIni, fecFin, this.entidadId)
      .subscribe((res) => {
        this.usuariosConvocatorias = res.items;
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

  cabeceraGeneral(fecIni: string, fecFin: string) {
    this.graficoService
      .cabecera('coordinador', this.f.condicion.value, null, fecIni, fecFin)
      .subscribe((res) => {
        this.numBases = res.numBases;
        this.numConvocatorias = res.numConvocatorias;
        this.numPerfiles = res.numPerfiles;
        this.numVacantes = res.numVacantes;
        this.numPostulantes = res.numPostulantes;
        this.numGestores = res.numGestores;
      });
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
      estado: 'NO APLICA',
      puestos: this.trabajos,
      empleo: this.regimen,
      practica: this.tipoPracticas,
      roles: this.roles,
      usuarios: this.gestores,
      usuariosDifusion: this.menor,
      usuariosConvocatoria: this.usuariosConvocatorias,
    };

    this.graficoService.downloadExcelAdmin(excel).subscribe(
      (res) => {
        const nameFile = `CuadroDeMandoAdminEntidad.xlsx`;
        const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toast.showToast('Ocurrió un error.', 'danger')
    );
  }

  datosExcel(fecha: any, condicion: string) {
    this.fechaExcel =
      moment(fecha.start).format('DD-MM-YYYY') +
      ' al ' +
      moment(fecha.end).format('DD-MM-YYYY');
    this.condicionExcel =
      condicion === null || condicion === ''
        ? 'TODOS'
        : this.condiciones.find((item) => condicion === item.codProg)
            .descripcion;
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }
}
