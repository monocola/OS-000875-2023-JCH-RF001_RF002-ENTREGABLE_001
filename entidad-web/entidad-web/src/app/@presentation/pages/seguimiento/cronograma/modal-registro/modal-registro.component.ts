import { Component, Inject, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { ThemePalette } from '@angular/material/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import moment from 'moment';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'serv-talento-modal-registro',
  templateUrl: './modal-registro.component.html',
  styleUrls: ['./modal-registro.component.scss'],
})
export class ModalRegistroComponent implements OnInit, OnDestroy {
  @ViewChild('picker') picker: any;
  form: FormGroup;
  public color: ThemePalette = 'primary';

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalRegistroComponent>,
    private basesService: BasesRepository,
    private toast: ToastService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.f.fechaIni.setValue(this.data.start);
    this.f.fechaFin.setValue(this.data.end);
    this.f.title.setValue(this.data.title);
    this.f.etapa.setValue(this.data.etapa);
    this.f.responsable.setValue(this.data.responsable);
    this.f.cronograma.setValue(this.data.cronograma);
    this.f.indexActividad.setValue(this.data.indexActividad);
    this.f.traslape.setValue(this.data.traslape);
    this.f.fecMax.setValue(this.data.fecMax);
    this.f.traslapeMin.setValue(this.data.traslapeMin);
    this.f.fecMin.setValue(this.data.fecMin);
  }

  initializeForm() {
    this.form = this.fb.group({
      title: '',
      fechaIni: '',
      fechaFin: '',
      horaIni: '',
      horaFin: '',
      etapa: '',
      etapaId: '',
      responsable: '',
      cronograma: '',
      indexActividad: '',
      traslape: '',
      fecMax: '',
      traslapeMin: '',
      fecMin: '',
    });
  }

  get f() {
    return this.form.controls;
  }

  guardar() {
    // validar fechas
    let fecIni = Utils.formatFechaDate(
      this.f.fechaIni.value,
      'YYYY-MM-DD HH:mm'
    );
    let fecFin = Utils.formatFechaDate(
      this.f.fechaFin.value,
      'YYYY-MM-DD HH:mm'
    );
    if (fecIni >= fecFin) {
      this.toast.showToast(
        'La fecha de inicio debe ser menor a la fecha de fin ',
        'danger'
      );
      return;
    }

    if (this.f.fecMax.value !== '') {
      let fecMaxEtapa = Utils.formatFechaDate(
        this.f.fecMax.value,
        'YYYY-MM-DD HH:mm'
      );

      if (fecIni >= fecMaxEtapa && !this.f.traslape.value) {
        this.toast.showToast(
          'La fecha seleccionada esta en el rango de la etapa siguiente. ',
          'danger'
        );
        return;
      }

      if (fecFin >= fecMaxEtapa && !this.f.traslape.value) {
        this.toast.showToast(
          'La fecha seleccionada esta en el rango de la etapa siguiente. ',
          'danger'
        );
        return;
      }
    }

    let fecMinEtapa = Utils.formatFechaDate(
      this.f.fecMin.value,
      'YYYY-MM-DD HH:mm'
    );

    if (fecMinEtapa >= fecIni && !this.f.traslapeMin.value) {
      this.toast.showToast(
        'La fecha seleccionada esta en el rango de la etapa anterior. ',
        'danger'
      );
      return;
    }

    if (fecMinEtapa >= fecFin && !this.f.traslapeMin.value) {
      this.toast.showToast(
        'La fecha seleccionada esta en el rango de la etapa anterior. ',
        'danger'
      );
      return;
    }

    /*if (this.f.traslape.value) {
      this.toast.showToast(
        'No se puede actualizar fecha, la actividad se traslapa con otra etapa.',
        'danger'
      );
      return;
    } else {
    }*/

    let HoraIni: string;
    let HoraFin: string;
    let MinIni: string;
    let MinFin = '00';
    let TimeIni = '00:00';
    let TimeFin = '00:00';

    if (this.f.fechaIni.value.getHours() < 10) {
      HoraIni = '0' + this.f.fechaIni.value.getHours();
    } else {
      HoraIni = this.f.fechaIni.value.getHours() + '';
    }

    if (this.f.fechaFin.value.getHours() < 10) {
      HoraFin = '0' + this.f.fechaFin.value.getHours();
    } else {
      HoraFin = this.f.fechaFin.value.getHours() + '';
    }
    MinIni =
      this.f.fechaIni.value.getMinutes() < 10
        ? '0' + this.f.fechaIni.value.getMinutes()
        : this.f.fechaIni.value.getMinutes();
    MinFin =
      this.f.fechaFin.value.getMinutes() < 10
        ? '0' + this.f.fechaFin.value.getMinutes()
        : this.f.fechaFin.value.getMinutes();

    TimeIni = HoraIni + ':' + MinIni;
    TimeFin = HoraFin + ':' + MinFin;

    this.f.cronograma.value.actividadDTOList[
      this.f.indexActividad.value
    ].fechaIni = this.f.fechaIni.value;
    this.f.cronograma.value.actividadDTOList[
      this.f.indexActividad.value
    ].fechaFin = this.f.fechaFin.value;
    this.f.cronograma.value.actividadDTOList[
      this.f.indexActividad.value
    ].horaIni = TimeIni;
    this.f.cronograma.value.actividadDTOList[
      this.f.indexActividad.value
    ].horaFin = TimeFin;

    let mayorDate = this.f.cronograma.value.actividadDTOList[0].fechaIni;
    let menorDate = this.f.cronograma.value.actividadDTOList[0].fechaFin;

    this.f.cronograma.value.actividadDTOList.forEach((element) => {
      let arrDate = element.fechaIni;
      let arrDateFin = element.fechaFin;
      if (arrDateFin > mayorDate) {
        mayorDate = arrDateFin;
      }

      if (arrDate < menorDate) {
        menorDate = arrDate;
      }
    });

    let fechaInicial = moment(menorDate).format('YYYY-MM-DDTHH:mm') + ':00';
    let fechaFinal = moment(mayorDate).format('YYYY-MM-DDTHH:mm') + ':00';

    const params = {
      reprogramacion: true,
      baseCronogramaDTOList: [
        {
          cronogramaId: this.f.cronograma.value.cronogramaId,
          baseId: this.f.cronograma.value.baseId,
          etapaId: this.f.cronograma.value.etapaId,
          descripcion: this.f.cronograma.value.descripcion,
          responsable: this.f.cronograma.value.responsable,
          periodoIni: fechaInicial,
          periodoFin: fechaFinal,
          estadoRegistro: this.f.cronograma.value.estadoRegistro,
          actividadDTOList: this.f.cronograma.value.actividadDTOList,
        },
      ],
    };
    
    this.basesService.editarCronogramaV2(params).subscribe((res) => {
      this.onNoClick(true);
    });
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  ngOnDestroy(): void {}

  closePicker() {
    this.picker.cancel();
  }
}
