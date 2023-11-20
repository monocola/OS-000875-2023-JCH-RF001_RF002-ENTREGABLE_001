import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CreacionBaseService } from '../../creacion-base/creacion-base.service';
import moment from 'moment';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-editar-cronograma',
  templateUrl: './editar-cronograma.component.html',
  styleUrls: ['./editar-cronograma.component.scss'],
})
export class EditarCronogramaComponent implements OnInit {
  @Input() title = 'Eliminar';
  @Input() bodyText = '¿Está seguro que desea continuar?';

  const = Const;
  mainForm: FormGroup;

  constructor(
    private matDialogRef: MatDialogRef<EditarCronogramaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel,
    private fb: FormBuilder,
    public helperService: CreacionBaseService
  ) {}

  ngOnInit(): void {
    this.initiliazeForm();
  }

  get f() {
    return this.mainForm.controls;
  }

  initiliazeForm() {
    this.mainForm = this.fb.group({
      etapaId: new FormControl(
        { value: this.data.cronograma.etapa.etapaId, disabled: true },
        Validators.required
      ),
      actividadId: [this.data.cronograma.actividad.actividadId],
      evaluacion: [
        this.data.cronograma.actividad.tipoActividad,
        Validators.required,
      ],
      descripcion: [
        this.data.cronograma.actividad.descripcion,
        Validators.required,
      ],
      responsable: [
        this.data.cronograma.actividad.responsable,
        Validators.required,
      ],
      fechaInicio: [
        moment(this.data.cronograma.actividad.fechaIni + ' ' + this.data.cronograma.actividad.horaIni)
          .format('YYYY-MM-DDTHH:mm:ss'),
        Validators.required,
      ],
      fechaFin: [
        moment(this.data.cronograma.actividad.fechaFin + ' ' + this.data.cronograma.actividad.horaFin)
          .format('YYYY-MM-DDTHH:mm:ss'),
        Validators.required,
      ],
    });
  }

  preventKeypress(event: KeyboardEvent) {
    event.preventDefault();
  }

  onCancel() {
    this.matDialogRef.close(false);
  }
  
  onSave() {
    this.matDialogRef.close({
      etapaId: this.mainForm.controls.etapaId.value,
      tipoEvaluacion: this.mainForm.controls.evaluacion.value,
      descripcion: this.mainForm.controls.descripcion.value,
      responsable: this.mainForm.controls.responsable.value,
      fechaInicio: this.mainForm.controls.fechaInicio.value,
      fechaFin: this.mainForm.controls.fechaFin.value,
      actividadId: this.mainForm.controls.actividadId.value,
    });
  }

  validarEtapaEvaluacion() {
    let returned: boolean = true;

    let etapaSelected = this.helperService.etapas.find(
      (x: any) => x.value === this.f.etapaId.value
    );

    if (
      etapaSelected &&
      etapaSelected.codProg === this.const.TIPO_ETAPA_CRONOGRAMA_EVALUACION
    ) {
      returned = false;
    }

    return returned;
  }
}

export interface ModalConfirmationModel {
  cronograma: any;
}
