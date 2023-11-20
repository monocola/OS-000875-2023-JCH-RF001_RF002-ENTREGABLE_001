import { Component, OnInit } from '@angular/core';
import { Sort } from '@angular/material/sort';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { sortDataTableComponent } from 'src/app/utils/general';
import { CreacionBaseService } from '../creacion-base.service';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EditarCronogramaComponent } from '../../components/editar-cronograma/editar-cronograma.component';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';
import moment from 'moment';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';

@Component({
  selector: 'serv-talento-step5',
  templateUrl: './step5.component.html',
})
export class Step5Component implements OnInit {
  const = Const;
  cronogramasColumns: TableColumn[];
  estados = [];
  mapCronogramas: Map<number, any[]> = new Map<number, any[]>();

  constructor(
    public helperService: CreacionBaseService,
    private basesService: BasesRepository,
    private dialog: MatDialog,
    private toastService: ToastService,
    public authRepository: AuthenticationRepository,
    private maestraService: MaestraRepository,
    private toast: ToastService
  ) { }

  get f() {
    return this.helperService.form5.controls;
  }

  ngOnInit(): void {
    moment.locale('es');
    this.initializeColumns();
    this.getEstados();
    this.getLista();
  }

  getLista() {
    if (!this.helperService.idBase) return;

    this.basesService
      .getListaCronogramasV2(this.helperService.idBase)
      .subscribe((res: any[]) => {
        this.helperService.listaCronogramas = res;
        this.helperService.listaCronogramas.forEach((item: any) => {
          item.periodoFin = this.getFormatDate(item.periodoFin);
          item.periodoIni = this.getFormatDate(item.periodoIni);
          this.mapCronogramas.set(item.etapaId, item.actividadDTOList);

          item.actividadDTOList.forEach((actividad: any) => {
            actividad.fechaFin = this.getFormatDate(actividad.fechaFin);
            actividad.fechaIni = this.getFormatDate(actividad.fechaIni);
          });
        });
      });
  }

  getFormatDate(date: any) {
    return moment(date).utc().format('YYYY-MM-DD');
  }

  validarEtapaEvaluacion() {
    let returned: boolean = true;

    let etapaSelected = this.helperService.etapas.find(
      (x: any) => x.value === this.f.etapa.value
    );

    if (
      etapaSelected &&
      etapaSelected.codProg === this.const.TIPO_ETAPA_CRONOGRAMA_EVALUACION
    ) {
      returned = false;
    }

    return returned;
  }

  initializeColumns() {
    this.cronogramasColumns = [
      {
        name: 'ETAPA',
        dataKey: 'etapa',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'ACTIVIDAD',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
        width: '35%',
      },
      {
        name: 'RESPONSABLE',
        dataKey: 'responsable',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'PERIODO',
        dataKey: 'periodo',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de etapas y cronograma';
    model.headers = ['ETAPA', 'ACTIVIDAD', 'RESPONSABLE', 'PERIODO'];
    model.keys = ['etapa', 'descripcion', 'responsable', 'periodo'];
    return model;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.helperService.listaCronogramas);
  }

  edit(cronograma: any) {
    const editarCronograma = this.dialog.open(EditarCronogramaComponent, {
      data: {
        cronograma: cronograma.data.restOfData,
      },
    });
    editarCronograma.afterClosed().subscribe((resEditar) => {
      if (!resEditar) return;

      const fechaInicio = moment(resEditar.fechaInicio);
      const fechaFin = moment(resEditar.fechaFin);

      if (fechaFin < fechaInicio) {
        this.toast.showToast(
          'Ingrese una Fecha de Fin mayor a la Fecha de Inicio',
          'danger',
          'Error'
        );
        return;
      }

      const validarCruces = this.validarCruces(
        fechaInicio,
        fechaFin,
        resEditar.etapaId,
        resEditar.actividadId
      );

      if (validarCruces.invalido) {
        const titulo: string = 'Cruce de fechas encontrado';
        const body: string = `El rango de fecha seleccionado se cruza con la actividad del ${validarCruces.response.fechaIni.format(
          'LL'
        )} - ${validarCruces.response.fechaFin.format(
          'LL'
        )}, ¿Esta seguro que desea aplazar la actividad?`;

        this.mostrarAlertaConfirmacion(titulo, body).then((res: boolean) => {
          if (res) {
            this.editActividad(cronograma, resEditar);
          }
        });
      } else {
        this.editActividad(cronograma, resEditar);
      }
    });
  }

  editActividad(cronograma: any, resEditar: any) {
    let lista = [
      {
        actividadId: cronograma.data.restOfData.actividad.actividadId,
        fechaIni: moment(resEditar.fechaInicio).format(),
        fechaFin: moment(resEditar.fechaFin).format(),
        horaIni: moment(resEditar.fechaInicio).format('HH:mm'),
        horaFin: moment(resEditar.fechaFin).format('HH:mm'),
        responsable: resEditar.responsable,
        descripcion: resEditar.descripcion,
        estadoRegistro: '1',
        tipoActividad: resEditar.tipoEvaluacion,
      },
    ];

    const params = {
      baseCronogramaDTOList: [
        {
          cronogramaId: cronograma.data.restOfData.etapa.cronogramaId,
          baseId: cronograma.data.restOfData.etapa.baseId,
          etapaId: cronograma.data.restOfData.etapa.etapaId,
          descripcion: cronograma.data.restOfData.etapa.descripcion,
          responsable: '',
          periodoIni: null,
          periodoFin: null,
          estadoRegistro: '1',
          actividadDTOList: lista,
        },
      ],
    };

    this.basesService.editarCronogramaV2(params).subscribe((res) => {
      if (res.status.success) {
        this.toastService.showToast(
          'Se editó el Cronograma correctamente',
          'success'
        );
      } else {
        this.toastService.showToast(
          'Ocurrió un error al editar el Cronograma',
          'danger'
        );
      }
      this.getLista();
    });
  }

  remove(cronograma: any) {
    const eliminarCronograma = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Cronograma',
        bodyText: '¿Está seguro que desea continuar?',
      },
    });

    eliminarCronograma.afterClosed().subscribe((resConfimacion) => {
      if (!resConfimacion) return;
      this.basesService
        .eliminarCronograma(cronograma.data.id)
        .subscribe((res) => {
          if (res.status.success) {
            this.toastService.showToast(
              'Se eliminó el Cronograma correctamente',
              'success'
            );
          } else {
            this.toastService.showToast(
              'Ocurrió un error al eliminar el Cronograma',
              'danger'
            );
          }
          this.getLista();
        });
    });
  }

  async mostrarAlertaConfirmacion(
    title: string,
    bodyText: string,
    visibleCancel: boolean = true
  ): Promise<boolean> {
    return await new Promise((resolve, reject) => {
      const eliminarCronograma = this.dialog.open(ModalConfirmationComponent, {
        data: {
          title: title,
          bodyText: bodyText,
          visibleCancel: visibleCancel,
        },
      });

      eliminarCronograma.afterClosed().subscribe((resConfimacion) => {
        resolve(resConfimacion);
      });
    });
  }

  getEstados() {
    this.maestraService
      .getMaestraDetalleByCod('TIP_ETA_PRO')
      .subscribe((res) => {
        this.estados = res;
      });
  }

  validarCruces(
    a_start: any,
    a_end: any,
    etapaId: number,
    actividadIdExcepto: number = null
  ) {
    if (!this.mapCronogramas.has(etapaId)) {
      return { invalido: false, response: null };
    }

    let array = this.mapCronogramas.get(etapaId);

    for (let index = 0; index < array.length; index++) {
      const b_start = moment(array[index].fechaIni);
      const b_end = moment(array[index].fechaFin);
      if (
        array[index].actividadId !== actividadIdExcepto &&
        this.dateRangeOverlaps(a_start, a_end, b_start, b_end)
      ) {
        return {
          invalido: true,
          response: { fechaIni: b_start, fechaFin: b_end },
        };
      }
    }

    return { invalido: false, response: null };
  }

  dateRangeOverlaps(a_start: any, a_end: any, b_start: any, b_end: any) {
    if (a_start <= b_start && b_start <= a_end) return true;
    if (a_start <= b_end && b_end <= a_end) return true;
    if (b_start < a_start && a_end < b_end) return true;
    return false;
  }

  validarEtapa() {
    this.helperService.form5.markAllAsTouched();
    if (this.helperService.form5.invalid) {
      return;
    }

    const fechaInicio = moment(this.f.fechaInicio.value);
    const fechaFin = moment(this.f.fechaFin.value);

    if (fechaFin < fechaInicio) {
      this.toast.showToast(
        'Ingrese una Fecha de Fin mayor a la Fecha de Inicio',
        'danger',
        'Error'
      );
      return;
    }

    const validarCruces = this.validarCruces(
      fechaInicio,
      fechaFin,
      this.f.etapa.value
    );

    if (validarCruces.invalido) {
      const tituli: string = 'Cruce de fechas encontrado';
      const body: string = `El rango de fecha seleccionado se cruza con la actividad del ${validarCruces.response.fechaIni.format(
        'LL'
      )} - ${validarCruces.response.fechaFin.format(
        'LL'
      )}, Por favor, ingrese un periodo de actividad correcto`;

      this.mostrarAlertaConfirmacion(tituli, body, false);
    } else {
      this.agregarEtapa();
    }
  }

  agregarEtapa() {
    let lista = [
      {
        actividadId: null,
        fechaIni: moment(this.f.fechaInicio.value).format(),
        fechaFin: moment(this.f.fechaFin.value).format(),
        horaIni: moment(this.f.fechaInicio.value).format('HH:mm'),
        horaFin: moment(this.f.fechaFin.value).format('HH:mm'),
        responsable: this.f.responsable.value,
        descripcion: this.f.actividad.value,
        estadoRegistro: '1',
        tipoActividad: this.validarValue(this.f.evaluacion.value),
      },
    ];

    const params = {
      baseCronogramaDTOList: [
        {
          cronogramaId: null,
          baseId: this.helperService.idBase,
          etapaId: this.f.etapa.value,
          descripcion: this.estados.find(
            (item) => item.maeDetalleId === this.f.etapa.value
          )?.descripcion,
          responsable: '',
          periodoIni: null,
          periodoFin: null,
          estadoRegistro: '1',
          actividadDTOList: lista,
        },
      ],
    };

    this.basesService.guardarBaseCronogramaV2(params).subscribe((res) => {
      if (res.status.success) {
        this.toast.showToast(
          'Se registró la etapa/actividad correctamente.',
          'success',
          'Atención'
        );
        this.f.etapa.setValue('');
        this.f.actividad.setValue('');
        this.f.evaluacion.setValue('');
        this.f.fechaInicio.setValue(null);
        this.f.fechaFin.setValue(null);
        this.f.responsable.setValue('');
        this.getLista();
        this.helperService.form5.markAsUntouched();
      } else {
        this.toast.showToast(
          'Ocurrió un error al guardar la actividad.',
          'danger',
          'Atención'
        );
      }
    });
  }

  validarValue(value: string) {
    if (value === undefined || value === null) {
      return null;
    }

    return value;
  }

  cancelar() {
    this.helperService.form5.reset();
    this.f.cronogramas.setValue([]);
  }
}
