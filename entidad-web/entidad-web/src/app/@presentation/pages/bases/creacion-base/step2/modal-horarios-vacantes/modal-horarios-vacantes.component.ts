import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  Inject,
  OnInit,
  ViewChild,
} from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { sortDataTableComponent } from 'src/app/utils/general';
import { CreacionBaseService } from '../../creacion-base.service';
import { hoursValues, minutesValues } from 'src/app/utils/values';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-modal-horarios-vacantes',
  changeDetection: ChangeDetectionStrategy.OnPush,
  templateUrl: './modal-horarios-vacantes.component.html',
})
export class ModalHorariosVacantesComponent implements OnInit {
  registerForm: FormGroup = null;
  editMode = false;
  editIndex = null;

  horariosColumn: TableColumn[] = [];
  horarios = [];
  horariosToDelete = [];
  horas = hoursValues;
  minutos = minutesValues;
  const = Const;

  @ViewChild('clearButton') clearButton: ElementRef;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private dialogRef: MatDialogRef<ModalHorariosVacantesComponent>,
    private fb: FormBuilder,
    public helperService: CreacionBaseService,
    private toastService: ToastService
  ) { }

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    if (this.data.horarios) {
      this.horarios = [...this.data.horarios];
    }
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.horarios);
  }

  get f() {
    return this.registerForm.controls;
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      frecuencia: [[], Validators.required],
      horaInicio: ['08', Validators.required],
      minutosInicio: ['00', Validators.required],
      horaFin: ['08', Validators.required],
      minutosFin: ['00', Validators.required],
      id: null,
    });
  }

  add() {
    this.registerForm.markAsTouched();
    if (this.registerForm.invalid) {
      this.toastService.showToast(
        'Debe completar los datos de la frecuencia',
        'danger'
      );
      return;
    }
    if (!this.checkHours(this.registerForm.value)) return;
    const value = this.registerForm.value;
    const frecuencias: any[] = value.frecuencia;
    this.initializeForm();
    this.clearButton.nativeElement.click();
    if (!this.editMode) {
      frecuencias.forEach((f) => {
        this.horarios = [
          {
            dia: f.descripcion,
            diaId: f.maeDetalleId,
            horaInicio: `${value.horaInicio}:${value.minutosInicio}`,
            horaFin: `${value.horaFin}:${value.minutosFin}`,
            id: f.id || null,
          },
          ...this.horarios,
        ];
      });
    } else {
      this.horarios.splice(this.editIndex, 0, {
        dia: value.frecuencia.descripcion,
        diaId: value.frecuencia.maeDetalleId,
        horaInicio: `${value.horaInicio}:${value.minutosInicio}`,
        horaFin: `${value.horaFin}:${value.minutosFin}`,
        id: value.id || null,
        orden: 9999,
      });
      this.horarios = this.horarios.filter((h) => h.orden !== this.editIndex);
      this.editMode = false;
      this.editIndex = null;
    }
    this.horarios.forEach((h, index) => (h.orden = index));
  }

  checkHours(body): boolean {
    let correct = true;
    const hourStart = body.horaInicio + ':' + body.minutosInicio;
    const hourEnd = body.horaFin + ':' + body.minutosFin;
    if (hourEnd <= hourStart) {
      this.toastService.showToast(
        'El horario final tiene que ser mayor al horario Inicial',
        'danger'
      );
      correct = false;
    }
    return correct;
  }

  guardarHorarios() {
    const data = {
      horarios: this.horarios,
      horariosToDelete: this.horariosToDelete,
    };
    this.onNoClick(data);
  }


  // ** Cuestiones de la tabla ** //

  onNoClick(newData: any = false) {
    this.dialogRef.close(newData);
  }

  editAction(event) {
    this.initializeForm();
    const hInicio = event.horaInicio.split(':');
    const hFin = event.horaFin.split(':');
    this.editMode = true;
    this.editIndex = event.orden;
    this.registerForm.patchValue({
      frecuencia: this.helperService.frecuencias.find(
        (f) => f.descripcion === event.dia
      ),
      horaInicio: hInicio[0],
      minutosInicio: hInicio[1],
      horaFin: hFin[0],
      minutosFin: hFin[1],
      id: null,
    });
  }

  removeAction(event) {
    if (event.id) {
      this.horariosToDelete = [...this.horariosToDelete, event];
    }
    this.horarios = this.horarios.filter((h) => h.orden !== event.orden);
    this.horarios.forEach((h, index) => (h.orden = index));
  }

  initializeColumns() {
    this.horariosColumn = [
      {
        name: 'DÍA',
        dataKey: 'dia',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'Hora/Entrada',
        dataKey: 'horaInicio',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
      {
        name: 'Hora/Salida',
        dataKey: 'horaFin',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
    ];
  }
}
