import { Component, forwardRef, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, FormControl } from '@angular/forms';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

import { NbCalendarRange, NbDateService } from '@nebular/theme';
import { Router } from '@angular/router';
import {
  Calendar,
  CalendarOptions,
  FullCalendarComponent,
} from '@fullcalendar/angular';
import dayGridPlugin from '@fullcalendar/daygrid'; // a plugin!
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import esLocale from '@fullcalendar/core/locales/es';
import listPlugin from '@fullcalendar/list';
import { ModalRegistroComponent } from './modal-registro/modal-registro.component';
import { MatDialog } from '@angular/material/dialog';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import moment from 'moment';
import tippy from 'tippy.js';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { SeguimientoComunicadoService } from '../../seguimiento-comunicado/seguimiento-comunicado.service';

@Component({
  selector: 'serv-talento-cronograma',
  templateUrl: './cronograma.component.html',
  styleUrls: ['./cronograma.component.scss'],
})
export class CronogramaComponent implements OnInit {
  @ViewChild('fullcalendar') calendarComponent: FullCalendarComponent;

  showContent: boolean = true;
  listaEvaluacionColumns = [];
  listaCronogramas: any[] = [];
  _listaCronogramas: any[] = [];
  evaluacionColumns: TableColumn[];
  filterForm: FormGroup;
  range: NbCalendarRange<Date>;
  valueEdit: number = 1;
  cronograma: any[] = [];
  etapas: any[] = [];
  checked = false;
  inicio: any = 3;
  fin: any = -3;

  estados = [];
  lista = [];
  listaTemporal = [];
  events = [];
  totaldias: number = 0;

  calendarOptions: CalendarOptions = {};

  calendarList: CalendarOptions = {};
  showAcciones: boolean = false;
  calendarioActivo: boolean = true;
  diaInicial: string = moment(Date.now()).format('YYYY-MM-DD');
  filtroForm: FormGroup;
  constructor(
    protected dateService: NbDateService<Date>,
    private fb: FormBuilder,
    private maestraService: MaestraRepository,
    private basesService: BasesRepository,
    public router: Router,
    private toast: ToastService,
    public dialog: MatDialog,
    public helperService: SeguimientoComunicadoService
  ) {
    forwardRef(() => Calendar);
  }

  ngOnInit(): void {
    if (!this.helperService.formConvocatoria) {
      this.helperService.initializeForm();
    }

    this.initFormFiltros();
    this.validarSesion();
  }

  get g() {
    return this.helperService.formConvocatoria.controls;
  }

  get f() {
    return this.filterForm.controls;
  }

  validarSesion() {
    if (this.g.baseId.value === 0) {
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
    } else {
      this.getEstados();
      this.getEventos();
      this.getHistoricos();
    }
  }

  initFormFiltros() {
    this.filterForm = new FormGroup({
      etapa: new FormControl(''),
      rangoFechas: new FormControl(null),
    });
  }

  getHistoricos() {
    this.basesService
      .getEtapas('TIP_ETA_PRO')
      .toPromise()
      .then((res: any[]) => {
        this.etapas = res;
      });

    this.basesService
      .getListaCronogramasHistorico(this.g.baseId.value)
      .toPromise()
      .then((res: any[]) => {
        this.listaCronogramas = [];
        this._listaCronogramas = [];

        let mapRes: Map<number, any> = new Map<number, any>();

        res.forEach((item: any) => {
          item.periodoFin = this.getFormatDate(item.periodoFin);
          item.periodoIni = this.getFormatDate(item.periodoIni);
          item.actividadDTOList.forEach((actividad: any) => {
            actividad.fechaFin = this.getFormatDate(actividad.fechaFin);
            actividad.fechaIni = this.getFormatDate(actividad.fechaIni);
          });

          if (mapRes.has(item.etapaId)) {
            item.actividadDTOList.forEach((actividad: any) => {
              mapRes.get(item.etapaId).actividadDTOList.push(actividad);
            });
          } else {
            mapRes.set(item.etapaId, item);
          }
        });

        mapRes.forEach((value: any, key: number) => {
          value.actividadDTOList.sort((item_a: any, item_b: any) => {
            return (
              new Date(item_a.fechaIni).getTime() -
              new Date(item_b.fechaIni).getTime()
            );
          });
        });

        mapRes.forEach((value: any, key: number) => {
          if (value.actividadDTOList.length > 0) {
            value.periodoIni = value.actividadDTOList[0].fechaIni;
            value.periodoFin =
              value.actividadDTOList[
                value.actividadDTOList.length - 1
              ].fechaFin;
          }
        });

        mapRes.forEach((value: any, key: number) => {
          this.listaCronogramas.push(value);
          this._listaCronogramas.push(value);
        });

        this.listaCronogramas = this.listaCronogramas.sort((a: any, b: any) => {
          return a.etapaId - b.etapaId;
        });

        this._listaCronogramas = this._listaCronogramas.sort(
          (a: any, b: any) => {
            return a.etapaId - b.etapaId;
          }
        );
      });
  }

  cargarCalendarioLista() {
    this.calendarList = {
      plugins: [listPlugin],
      timeZone: 'UTC',
      initialDate: this.diaInicial,
      initialView: 'timeProceso',
      editable: false,
      locale: 'es',
      locales: [esLocale],
      height: '600px',
      views: {
        timeProceso: {
          type: 'listWeek',
          duration: { days: 7 },
        },
      },
      headerToolbar: {
        left: '',
        center: 'title',
        right: '',
      },
      events: [],
    };
  }

  cargarCalendario() {
    this.calendarOptions = {
      plugins: [timeGridPlugin, dayGridPlugin, interactionPlugin],
      eventDisplay: 'block',
      initialView: 'dayGridMonth',
      initialDate: this.diaInicial,
      editable: true,
      locale: 'es',
      locales: [esLocale],
      headerToolbar: {
        left: 'today prev,next',
        center: 'title',
        right: 'dayGridMonth,timeGridWeek,timeGridDay',
      },
      events: [],
      eventClick: (args) => {
        this.OpenModal(args.event);
      },
      eventDidMount: (info) => {
        const content: string = `
          <div style="border: 1px solid #000000;-moz-border-radius: 7px;-webkit-border-radius: 7px;padding: 10px;background-color: skyblue;width: 150px;">
            <h6  style="text-align: center;">
              ${info.event.extendedProps.etapaDesc}
            </h6>
            <p>
              ${info.event.title}
            </p>
            <p>
              Responsable: ${info.event.extendedProps.actividad.responsable}
            </p>
          </div>
        `;
        tippy(info.el, {
          content: content,
          placement: 'top',
          allowHTML: true,
        });
      },
      eventDrop: (args) => {
        this.ActualizarCalendario(args);
      },
    };
  }

  validadorFechas(args: any): boolean {
    let fecIni = moment(args.event.start).format('YYYY-MM-DD HH:mm');
    let fecFin = moment(args.event.end).format('YYYY-MM-DD HH:mm');

    let fecMaxEtapa =
      args.event.extendedProps.fecMax !== ''
        ? moment(args.event.extendedProps.fecMax).format('YYYY-MM-DD HH:mm')
        : '';
    let fecMinEtapa =
      args.event.extendedProps.fecMin !== ''
        ? moment(args.event.extendedProps.fecMin).format('YYYY-MM-DD HH:mm')
        : '';

    if (fecMaxEtapa !== '') {
      if (fecIni >= fecMaxEtapa && !args.event.extendedProps.traslape) {
        this.toast.showToast(
          'La fecha seleccionada esta en el rango de la siguiente etapa. ',
          'danger'
        );
        return true;
      }

      if (fecFin >= fecMaxEtapa && !args.event.extendedProps.traslape) {
        this.toast.showToast(
          'La fecha seleccionada esta en el rango de la siguiente etapa. ',
          'danger'
        );
        return true;
      }
    }

    if (fecMinEtapa >= fecIni && !args.event.extendedProps.traslapeMin) {
      this.toast.showToast(
        'La fecha seleccionada esta en el rango de la etapa anterior. ',
        'danger'
      );
      return true;
    }

    if (fecMinEtapa >= fecFin && !args.event.extendedProps.traslapeMin) {
      this.toast.showToast(
        'La fecha seleccionada esta en el rango de la etapa anterior. ',
        'danger'
      );
      return true;
    }
    return false;
  }

  ActualizarCalendario(args: any) {
    if (this.validadorFechas(args)) {
      this.getEventos();
      this.getHistoricos();
      return;
    }

    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Cambiar Fecha de Actividad',
        bodyText: '¿Está seguro de continuar?',
        rutaImagen: 'assets/images/icons/editar.png',
      },
    });

    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        let HoraIni: string;
        let HoraFin: string;
        let MinIni: string;
        let MinFin = '00';
        let TimeIni = '00:00';
        let TimeFin = '00:00';
        if (args.event.start.getHours() < 10) {
          HoraIni = '0' + args.event.start.getHours();
        } else {
          HoraIni = args.event.start.getHours() + '';
        }

        if (args.event.end.getHours() < 10) {
          HoraFin = '0' + args.event.end.getHours();
        } else {
          HoraFin = args.event.end.getHours() + '';
        }

        MinIni =
          args.event.start.getMinutes() < 10
            ? '0' + args.event.start.getMinutes()
            : args.event.start.getMinutes();
        MinFin =
          args.event.end.getMinutes() < 10
            ? '0' + args.event.end.getMinutes()
            : args.event.end.getMinutes();

        TimeIni = HoraIni + ':' + MinIni;
        TimeFin = HoraFin + ':' + MinFin;

        let cronograma = args.event.extendedProps.cronograma;
        cronograma.actividadDTOList[
          args.event.extendedProps.indexActividad
        ].fechaIni = args.event.start;
        cronograma.actividadDTOList[
          args.event.extendedProps.indexActividad
        ].fechaFin = args.event.end;
        cronograma.actividadDTOList[
          args.event.extendedProps.indexActividad
        ].horaIni = TimeIni;
        cronograma.actividadDTOList[
          args.event.extendedProps.indexActividad
        ].horaFin = TimeFin;

        let mayorDate = cronograma.actividadDTOList[0].fechaIni;
        let menorDate = cronograma.actividadDTOList[0].fechaFin;

        cronograma.actividadDTOList.forEach((element: any) => {
          let arrDate = element.fechaIni;
          let arrDateFin = element.fechaFin;
          if (arrDateFin > mayorDate) {
            mayorDate = arrDateFin;
          }

          if (arrDate < menorDate) {
            menorDate = arrDate;
          }
        });

        cronograma.periodoIni =
          moment(menorDate).format('YYYY-MM-DDTHH:mm') + ':00';
        cronograma.periodoFin =
          moment(mayorDate).format('YYYY-MM-DDTHH:mm') + ':00';

        const params = {
          reprogramacion: true,
          baseCronogramaDTOList: [cronograma],
        };

        this.basesService.editarCronogramaV2(params).subscribe(() => {
          this.toast.showToast(
            'Se actualizó la actividad.',
            'success',
            'Atención'
          );
          this.getEventos();
          this.getHistoricos();
        });
      }
    });
  }

  getEventos() {
    this.events = [];
    this.basesService
      .getListaCronogramasV2(this.g.baseId.value)
      .subscribe((res) => {
        this.lista = this.formatDateRes(res);
        if (this.lista.length <= 0) {
          this.cargarCalendario();
          this.cargarCalendarioLista();
          this.calendarioActivo = !this.calendarioActivo;
          return;
        }

        if (this.calendarioActivo) {
          this.calendarioActivo = !this.calendarioActivo;
          this.diaInicial = moment(this.lista[0].periodoIni).format(
            'YYYY-MM-DD'
          );
          this.cargarCalendario();
          this.cargarCalendarioLista();
        }

        if (this.lista.length > 0) {
          this.diaInicial = moment(this.lista[0].periodoIni)
            .utc()
            .format('YYYY-MM-DD');
          let ini = moment(this.getFormatDate(this.lista[0].periodoIni));
          let fin = moment(
            this.getFormatDate(this.lista[this.lista.length - 1].periodoFin)
          );

          this.totaldias = fin.diff(ini, 'days') + 1;

          this.calendarList.views = {
            timeProceso: {
              type: 'listWeek',
              duration: { days: this.totaldias },
            },
          };
        }

        this.lista.forEach((element: any) => {
          this.listaTemporal = element.actividadDTOList;
          this.listaTemporal.sort((a: any, b: any) => {
            if (a.fechaIni > b.fechaIni) {
              return 1;
            }
            if (a.fechaIni < b.fechaIni) {
              return -1;
            }
            return 0;
          });
          this.listaTemporal.forEach((e) => {
            let temp = {
              title: e.descripcion,
              start: e.fechaIni + ' ' + e.horaIni + ':00',
              end: e.fechaFin + ' ' + e.horaFin + ':00',
              extendedProps: {
                cronograma: element,
                actividad: e,
                indexActividad: this.listaTemporal.indexOf(e),
                etapaDesc: element.descripcion,
                traslape: this.trasponer(element.codProg),
                fecMax: this.fechaMaxima(element.codProg),
                fecMin: this.fechaMinima(element.codProg),
                traslapeMin: this.trasponerMin(element.codProg),
              },
              backgroundColor: this.validarColor(element.codProg),
            };
            this.events.push(temp);
          });
        });
        this.calendarOptions.events = this.events;
        this.calendarList.events = this.events;
      });
  }

  formatDateRes(res: any[]) {
    res.forEach((etapa: any) => {
      etapa.actividadDTOList.forEach((actividad: any) => {
        actividad.fechaFin = this.getFormatDate(actividad.fechaFin);
        actividad.fechaIni = this.getFormatDate(actividad.fechaIni);
      });
    });

    return res;
  }

  getFormatDate(date: any) {
    return moment(date).utc().format('YYYY-MM-DD');
  }

  trasponer(codigo: string): boolean {
    let salida: boolean = false;
    let cod = codigo;
    if (cod === '1') {
      salida = true;
    }
    return salida;
  }

  trasponerMin(codigo: string): boolean {
    let salida: boolean = false;
    let cod = codigo;
    if (cod === '1' || cod === '2') {
      salida = true;
    }
    return salida;
  }

  fechaMaxima(codigo: string): string {
    let salida = '';
    let cod = codigo;
    if (+cod !== 4) {
      salida = this.lista[+cod - 1].actividadDTOList[0].fechaIni;
    }
    return salida;
  }

  fechaMinima(codigo: string): string {
    let salida = '';
    let cod = codigo;

    if (+cod !== 1) {
      salida = moment(
        this.lista[+cod - 2].actividadDTOList[
          this.lista[+cod - 2].actividadDTOList.length - 1
        ].fechaFin
      ).format('YYYY-MM-DD HH:mm:ss');
    }
    return salida;
  }

  getEstados() {
    this.maestraService
      .getMaestraDetalleByCod('TIP_ETA_PRO')
      .subscribe((res) => {
        this.estados = res;
      });
  }

  validarColor(codigo: string): string {
    let codEstado = codigo;
    let color = '';
    if (codEstado === '1') {
      color = '#7c7c7c';
    }
    if (codEstado === '2') {
      color = '#c4302b';
    }
    if (codEstado === '3') {
      color = '#6441a5';
    }
    if (codEstado === '4') {
      color = '#ea5252';
    }
    return color;
  }

  obtenerEstado(codigo: number): string {
    return this.estados.find((item) => item.maeDetalleId === codigo)
      .descripcion;
  }

  OpenModal(args) {
    const modalNivelEducativo = this.dialog.open(ModalRegistroComponent, {
      width: '40rem',
      data: {
        start: args.start,
        end: args.end,
        title: args.title,
        etapaId: args.extendedProps.id,
        etapa: args.extendedProps.etapaDesc,
        responsable: args.extendedProps.actividad.responsable,
        cronograma: args.extendedProps.cronograma,
        indexActividad: args.extendedProps.indexActividad,
        traslape: args.extendedProps.traslape,
        fecMax: args.extendedProps.fecMax,
        traslapeMin: args.extendedProps.traslapeMin,
        fecMin: args.extendedProps.fecMin,
      },
    });
    modalNivelEducativo.afterClosed().subscribe((res) => {
      if (res) {
        this.toast.showToast(
          'Se modificó la actividad seleccionada.',
          'success',
          'Atención'
        );
        this.getEventos();
        this.getHistoricos();
      }
    });
  }

  limpiar() {
    this.listaCronogramas = this._listaCronogramas;
    this.filterForm.reset();
  }

  filtrarHistorico() {
    this.listaCronogramas = this._listaCronogramas;
    this.listaCronogramas = this.listaCronogramas.filter((item: any) => {
      return (
        this.filtrarEtapa(this.filterForm.value.etapa, item.etapaId) &&
        this.filtrarDateRange(
          this.filterForm.value.rangoFechas,
          moment(item.periodoIni)
        )
      );
    });
  }

  filtrarEtapa(formEtapaId: any, etapaId: any) {
    if (formEtapaId === '') {
      return true;
    }

    return formEtapaId === etapaId;
  }

  filtrarDateRange(rangoFechas: any, date: any) {
    if (rangoFechas === null) {
      return true;
    }

    const start = moment(rangoFechas.start);
    const end = moment(rangoFechas.end);

    if (start === null || end === null) {
      return true;
    }

    return date >= start && date <= end;
  }
}
