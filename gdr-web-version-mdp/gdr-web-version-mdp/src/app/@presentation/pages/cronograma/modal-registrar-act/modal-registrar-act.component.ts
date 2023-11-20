import { Component, OnInit, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { DatePipe } from '@angular/common';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { CronogramaRepository } from '../../../../@domain/repository/cronograma.repository';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { CicloService } from '../../../../@data/services/ciclo.service';
import { forkJoin } from 'rxjs';
import { ToastService } from '../../../@common-components/toast';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { Cronograma } from '../../../../@data/model/cronograma';
import moment from 'moment';

@Component({
  selector: 'serv-talento-modal-registrar-act',
  templateUrl: './modal-registrar-act.component.html',
  styleUrls: ['./modal-registrar-act.component.scss'],
})
export class ModalRegistrarActComponent implements OnInit {
  filterForm: FormGroup;
  fechaInicio: Date;
  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<ModalRegistrarActComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private datePipe: DatePipe,
    private cronogramaRepository: CronogramaRepository,
    private maeParametroRepository: MaestraParametroRepository,
    private cicloService: CicloService,
    private toastService: ToastService,
    private authenticationService: AuthenticationRepository
  ) {}

  responsableslistSelect: MaestraParametro[] = [];
  responsable: MaestraParametro[];
  etapa: MaestraParametro[];
  profile = this.authenticationService.getCurrentUserValue;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc;
  cicloDefault;
  listResponsable: any[] = [];
  listResponsableEdit: number[] = [];
  listRespoEditSel: number[] = [];
  createMode = true;
  minDate: {};

  ngOnInit(): void {
    this.createMode = this.data.createMode;
    this.loadCombox();
    this.initializeForm();
    this.setCiclo();
  }

  get f() {
    return this.filterForm.controls;
  }

  loadCombox() {
    const getEtapa = this.maeParametroRepository.getMaestraParametro(
      'ETAPA_GDR'
    );
    const getResponsable = this.maeParametroRepository.getMaestraParametro(
      'RESPONSABLE_GDR'
    );
    forkJoin([getEtapa, getResponsable]).subscribe(
      (results) => {
        this.etapa = results[0];
        this.responsable = results[1];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      cronogramaId: this.ciclo.cronogramaId,
      etapa: ['', [Validators.required]],
      descripcion: ['', [Validators.required]],
      responsables: [this.listResponsableEdit, [Validators.required]],
      fechaInicio: ['', [Validators.required]],
      fechaFin: ['', [Validators.required]],
      descripcionEtapa: [''],
      listResponsable: [''],
    });

    setTimeout(() => {
      const input = document.getElementById('fechaInicio');

      input.setAttribute('maxlength', '10');

      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };

      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };

      const fechaFin = document.getElementById('fechaFin');

      fechaFin.setAttribute('maxlength', '10');

      fechaFin.onkeydown = (e: any) => {
        return this.isNumeric(fechaFin, e.keyCode);
      };

      fechaFin.onkeyup = (e: any) => {
        this.validateDateFormat(fechaFin, e.keyCode);
      };
    }, 250);

    if (this.data.dataToEdit) {
      setTimeout(() => {
        this.updateForm();
      }, 0);
    }
  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
    console.log(keyCode);

    if (keyCode === 16) {
      this.isShift = true;
    }

    if (
      ((keyCode >= 48 && keyCode <= 57) ||
        keyCode === 8 ||
        keyCode === 46 ||
        keyCode === 37 ||
        keyCode === 39 ||
        (keyCode >= 96 && keyCode <= 105)) &&
      this.isShift === false
    ) {
      if (
        (input.value.length === 2 || input.value.length === 5) &&
        keyCode !== 8 &&
        keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    } else {
      // Es invalido
    }
  }

  updateForm() {
    const cronograma = this.data.dataToEdit;

    let respon = this.data.dataToEdit.responsables;
    for (let i = 0; i < respon.length; i++) {
      this.responsableslistSelect.push(
        this.data.responsable.find(
          (item) => item.codigoNumero === respon[i].responsableId
        )
      );
    }
    this.responsableslistSelect.forEach((item) => {
      this.listResponsableEdit.push(item.codigoNumero);
    });

    let fechaInicio = null;
    if (cronograma.fechaInicio !== null) {
      fechaInicio = moment(cronograma.fechaInicio, 'DD/MM/YYYY').toDate();
    }

    let fechaFin = null;
    if (cronograma.fechaFin !== null) {
      fechaFin = moment(cronograma.fechaFin, 'DD/MM/YYYY').toDate();
    }

    this.filterForm.patchValue({
      cronogramaId: this.ciclo.cronogramaId,
      etapa: cronograma.etapaId,
      descripcion: cronograma.descripcionActividad,
      responsables:
        this.listResponsableEdit != null &&
        this.listResponsableEdit.length !== 0
          ? this.listResponsableEdit
          : [],
      fechaInicio: fechaInicio,
      fechaFin: fechaFin,
    });

    this.cambioResponsables(this.listResponsableEdit);
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  cambioResponsables(values: number[]) {
    if (this.data.createMode) {
      this.responsableslistSelect = this.responsable.filter((item) =>
        values.includes(item.codigoNumero)
      );
      this.listResponsable = [];
      this.responsableslistSelect.forEach((item) =>
        this.listResponsable.push({
          responsableId: item.codigoNumero,
          descripcionResponsable: item.valorTexto,
        })
      );
    } else {
      this.responsableslistSelect = [];
      this.responsableslistSelect = this.data.responsable.filter((item) =>
        values.includes(item.codigoNumero)
      );
      this.listResponsable = [];
      this.responsableslistSelect.forEach((item) =>
        this.listResponsable.push({
          responsableId: item.codigoNumero,
          descripcionResponsable: item.valorTexto,
        })
      );
    }
  }

  saveActividad() {
    this.filterForm.markAllAsTouched();
    let frm = this.filterForm.getRawValue();

    if (frm.fechaInicio > frm.fechaFin) {
      this.toastService.showToast(
        'La fecha de inicio debe ser menor a la fecha de fin.',
        'danger',
        'Error'
      );
      return;
    }

    frm.listResponsable = this.listResponsable;
    frm.descripcionEtapa = this.etapa.find(
      (item) => item.codigoNumero === frm.etapa
    ).valorTexto;
    if (!this.listResponsable) {
      this.listResponsable = null;
    }
    let body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        cronogramaId: frm.cronogramaId,
        etapaId: frm.etapa,
        descripcionEtapa: frm.descripcionEtapa,
        descripcionActividad: frm.descripcion,
        fechaInicio: this.datePipe.transform(frm.fechaInicio, 'dd/MM/yyyy'),
        fechaFin: this.datePipe.transform(frm.fechaFin, 'dd/MM/yyyy'),

        // fechaInicio: Utils.parseFechaString(this.f.fechaInicio.value),
        //  fechaFin: Utils.parseFechaString(this.f.fechaFin.value),

        // fechaInicio: Utils.formatFechaString(fechaInicio, 'DD/MM/YYYY' ),
        // fechaFin: Utils.formatFechaString( fechaFin, 'DD/MM/YYYY' ),

        // fechaInicio: this.datePipe.transform(frm.fechaInicio, 'dd/MM/yyyy'),
        // fechaFin: this.datePipe.transform(frm.fechaFin, 'dd/MM/yyyy'),
        // fechaInicio: new Date (frm.fechaInicio),
        // fechaFin: new Date (frm.fechaFin),
        responsables: frm.listResponsable,
      },
    };
    if (this.filterForm.valid) {
      this.cronogramaRepository.registerOrUpdateAct(body, true).subscribe(
        (res) => {
          if (res) {
            this.toastService.showToast(
              'Se realizó el registro exitosamente',
              'success'
            );
            this.onNoClick(true);
          } else {
            this.toastService.showToast(
              'Hubo un error al registrar la actividad',
              'danger'
            );
            this.onNoClick(false);
          }
        },
        (err) => this.toastService.showToast(err.message, 'danger')
      );
    }
  }

  editActividad() {
    this.filterForm.markAllAsTouched();
    let frm = this.filterForm.getRawValue();
    console.log (frm);
    if (frm.fechaInicio > frm.fechaFin) {
      this.toastService.showToast(
        'La fecha de inicio debe ser menor a la fecha de fin.',
        'danger',
        'Error'
      );
      return;
    }

    const actividadId = this.data.dataToEdit.actividadId;
    frm.listResponsable = this.listResponsable;
    frm.descripcionEtapa = this.etapa.find(
      (item) => item.codigoNumero === frm.etapa
    ).valorTexto;
    if (!this.listResponsable) {
      this.listResponsable = null;
    }
    let body = {
      trace: {
        traceId: 'string',
      },
      payload: {
        actividadId: actividadId,
        cronogramaId: frm.cronogramaId,
        etapaId: frm.etapa,
        descripcionEtapa: frm.descripcionEtapa,
        descripcionActividad: frm.descripcion,
        fechaInicio: this.datePipe.transform(frm.fechaInicio, 'dd/MM/yyyy'),
        fechaFin: this.datePipe.transform(frm.fechaFin, 'dd/MM/yyyy'),
        responsables: frm.listResponsable,
      },
    };
    if (this.filterForm.valid) {
      this.cronogramaRepository.registerOrUpdateAct(body, false).subscribe(
        (res) => {
          if (res) {
            // La sede se ha editado correctamente
            this.toastService.showToast(
              'Se realizó la actualización exitosamente',
              'success',
              'Correcto'
            );
            this.onNoClick(true);
          } else {
            this.toastService.showToast(
              'Hubo un error al editar la sede',
              'danger'
            );
            this.onNoClick(false);
          }
        },
        (err) => this.toastService.showToast(err.message, 'danger')
      );
    }
  }
}

export interface DataModel {
  createMode: boolean;
  dataToEdit: Cronograma;
  estados: any[];
  responsable: any[];
}
