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
  selector: 'serv-talento-gestor',
  templateUrl: './gestor.component.html',
  styleUrls: ['./gestor.component.scss'],
})
export class GestorComponent implements OnInit {
  filterForm: FormGroup;
  rangePickerStatus: string = 'basic';

  barWidth: string = '100%';

  numGestores: number = 0;
  numConvocatorias: number = 0;
  numPerfiles: number = 0;
  numVacantes: number = 0;
  numPostulantes: number = 0;

  fechaExcel: string = '';
  condicionExcel: string = '';
  estadoExcel: string = '';

  estados = [];

  condiciones = [];

  stackedData: any;

  stackedOptions: any;

  data: any;

  options: any;

  dataPracticantes: any;
  optionsPracticantes: any;

  dataEstadoPostulante: any;
  optionsEstadoPostulante: any;

  horizontalOptions: any;
  basicData: any;

  dataMenorPerfiles: any;
  optionsMenorPerfiles: any;

  trabajos = [];

  regimen = [];

  tipoPracticas = [];

  postulantes = [];

  mayor = [];

  menor = [];

  TopPerfilPorcentaje: '0 %';
  LowPerfilPorcentaje: '0 %';

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
      estado: null,
      fecha: null,
    });
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

  cargarGraficoEstadoPostulante() {
    let descripcion: string[] = [];
    let valor: number[] = [];
    let suma = 0;

    this.postulantes.forEach((e) => {
      descripcion.push(e.nombre);
      valor.push(e.data);
      suma += e.data;
    });

    this.dataEstadoPostulante = {
      labels: descripcion,
      datasets: [
        {
          data: valor,
          backgroundColor: ['#00c3ff', '#008abd', '#7a3900', '#cd6600'],
          hoverBackgroundColor: ['#00c3ff', '#008abd', '#7a3900', '#cd6600'],
        },
      ],
    };
    this.optionsEstadoPostulante = {
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
              text: 'postulantes',
              font: {
                size: '50',
              },
              color: 'black',
            },
          ],
        },
        labels: [
          {
            render: 'label',
            position: 'outside',
            fontColor: ['black', 'black', 'black', 'black'],
          },
          {
            render: 'percentage',
            fontColor: ['white', 'white', 'white', 'white'],
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

  cargarTopPerfiles() {
    let descripcion: string[] = [];
    let valor: number[] = [];

    this.mayor.forEach((e) => {
      descripcion.push(e.nombre + ' (' + e.data + ')');
      valor.push(e.data);
    });

    this.basicData = {
      labels: descripcion,
      datasets: [
        {
          label: '',
          backgroundColor: '#42A5F5',
          data: valor,
        },
      ],
    };

    this.horizontalOptions = {
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

    console.log(this.trabajos);

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

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
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

  delay(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  cargarCombox() {
    this.maestraService
      .getMaestraDetalleByCod('TIP_EST_CONV')
      .subscribe((res) => {
        this.estados = res;
      });

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
      this.f.estado.setValue(
        this.f.estado.value === '' ? null : this.f.estado.value
      );
      this.datosExcel(
        this.f.fecha.value,
        this.f.condicion.value,
        this.f.estado.value
      );
      this.cabeceraGeneral(fecIni, fecFin);
      this.graficosPuestosTotales(fecIni, fecFin);
      this.graficosRegimenPractica(fecIni, fecFin);
      this.graficoEstadoPostulante(fecIni, fecFin);
      this.graficoRankingPerfiles(fecIni, fecFin);
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
    this.graficosPuestosTotales(fecIni, fecFin);
    this.graficosRegimenPractica(fecIni, fecFin);
    this.graficoEstadoPostulante(fecIni, fecFin);
    this.graficoRankingPerfiles(fecIni, fecFin);
  }

  datosExcel(fecha: any, condicion: string, estado: string) {
    this.fechaExcel =
      moment(fecha.start).format('DD-MM-YYYY') +
      ' al ' +
      moment(fecha.end).format('DD-MM-YYYY');
    this.condicionExcel =
      condicion === null || condicion === ''
        ? 'TODOS'
        : this.condiciones.find((item) => condicion === item.codProg)
            .descripcion;

    this.estadoExcel =
      estado === null || estado === ''
        ? 'TODOS'
        : this.estados.find((item) => estado === item.codProg).descripcion;
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

  cabeceraGeneral(fecIni: string, fecFin: string) {
    this.graficoService
      .cabecera(
        'gestor',
        this.f.condicion.value,
        this.f.estado.value,
        fecIni,
        fecFin
      )
      .subscribe((res) => {
        this.numGestores = res.numGestores;
        this.numConvocatorias = res.numConvocatorias;
        this.numPerfiles = res.numPerfiles;
        this.numVacantes = res.numVacantes;
        this.numPostulantes = res.numPostulantes;
      });
  }

  graficosPuestosTotales(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoBarrasPuestos(
        '',
        this.f.condicion.value,
        this.f.estado.value,
        fecIni,
        fecFin
      )
      .subscribe((res) => {
        console.log(res);
        this.trabajos = res.respPuestosTotalesDetalles;
        this.cargarGraficoTrabajos();
      });
  }

  graficosRegimenPractica(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoDonutPuestos(
        '',
        this.f.condicion.value,
        this.f.estado.value,
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

  graficoEstadoPostulante(fecIni: string, fecFin: string) {
    this.graficoService
      .graficoDonutEstadoPostulante(
        this.f.condicion.value,
        this.f.estado.value,
        fecIni,
        fecFin
      )
      .subscribe((res) => {
        this.postulantes = res.postulante;
        this.cargarGraficoEstadoPostulante();
      });
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

  print() {
    let excel = {
      fecha: this.fechaExcel,
      condicion: this.condicionExcel,
      estado: this.estadoExcel,
      empleo: this.regimen,
      practica: this.tipoPracticas,
      estadopostulantes: this.postulantes,
      puestos: this.trabajos,
      topPerfiles: this.mayor,
      lowPerfiles: this.menor,
    };
    this.graficoService.downloadExcelGestor(excel).subscribe(
      (res) => {
        const nameFile = `CuadroDeMandoGestor.xlsx`;
        const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toast.showToast('Ocurrió un error.', 'danger')
    );
  }
}
