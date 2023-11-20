import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CreacionFormBaseService } from '../../creacion-form-base/creacion-form-base.service';

@Component({
  selector: 'serv-talento-modal-creacion-bonificacion',
  templateUrl: './modal-creacion-bonificacion.component.html',
  styleUrls: ['./modal-creacion-bonificacion.component.scss'],
})
export class ModalCreacionBonificacionComponent implements OnInit {
  modalCreacionForm = this.fb.group({
    descripcion: ['', Validators.required],
    niveles: ['', Validators.required],
    aplicaSobre: [''],
    porcentaje: ['', Validators.required],
    id: [null],
    estado: 1,
  });

  constructor(
    private fb: FormBuilder,
    public helperService: CreacionFormBaseService,
    private dialogRef: MatDialogRef<ModalCreacionBonificacionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    if (this.data.form) {
      const form = this.data.form;
      this.modalCreacionForm.patchValue({
        descripcion: form.descripcion,
        niveles: this.helperService.nivelesBonificaciones.find(
          (a) => a.maeDetalleId === form.niveles.maeDetalleId
        ),
        aplicaSobre: this.helperService.aplicaSobre.find(
          (a) => a.maeDetalleId === form.aplicaSobre.maeDetalleId
        ),
        porcentaje: form.porcentaje,
        id: form.id,
      });
    }
  }

  returnNiveles() {
    let nivelesSelected: number[] =
      this.data?.detalles?.map((d) => d.niveles?.maeDetalleId || null) || [];
    if (this.f.niveles.value) {
      const index = nivelesSelected.indexOf(this.f.niveles.value.maeDetalleId);
      nivelesSelected.splice(index, 1);
      return this.helperService.nivelesBonificaciones.filter(
        (b) => !nivelesSelected.includes(b.maeDetalleId)
      );
    } else {
      return this.helperService.nivelesBonificaciones.filter(
        (b) => !nivelesSelected.includes(b.maeDetalleId)
      );
    }
  }

  get f() {
    return this.modalCreacionForm.controls;
  }

  validatePercentage() {
    const value = Number(this.f.porcentaje.value);
    if (value > 100) {
      this.f.porcentaje.patchValue('100');
    } else if (this.f.porcentaje.value.length > 0) {
      this.f.porcentaje.patchValue(value);
    }
  }

  onNoClick(value?) {
    this.dialogRef.close(value ? value : false);
  }

  saveBonificacion() {
    this.modalCreacionForm.markAllAsTouched();
    if (this.modalCreacionForm.valid) {
      this.onNoClick(this.modalCreacionForm.getRawValue());
    }
  }
}
