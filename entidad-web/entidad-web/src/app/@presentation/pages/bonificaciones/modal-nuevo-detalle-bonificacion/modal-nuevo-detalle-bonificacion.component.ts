import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BonificacionesHelperService } from '../bonificaciones-helper.service';

@Component({
  selector: 'serv-talento-modal-nuevo-detalle-bonificacion',
  templateUrl: './modal-nuevo-detalle-bonificacion.component.html',
})
export class ModalNuevoDetalleBonificacionComponent implements OnInit {
  modalNuevoDetalleForm = this.fb.group({
    descripcion: ['', Validators.required],
    niveles: ['', Validators.required],
    aplicaSobre: [''],
    porcentaje: ['', Validators.required],
    id: null,
    estado: 1,
  });

  constructor(
    public helperService: BonificacionesHelperService,
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalNuevoDetalleBonificacionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    if (this.data.form) {
      const form = this.data.form;
      this.modalNuevoDetalleForm.patchValue({
        descripcion: form.descripcion,
        niveles: this.helperService.niveles.find(
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

  onNoClick(value?) {
    this.dialogRef.close(value ? value : false);
  }

  get f() {
    return this.modalNuevoDetalleForm.controls;
  }

  returnNiveles() {
    let nivelesSelected: number[] =
      this.data?.detalles?.map((d) => d.niveles?.maeDetalleId || null) || [];
    if (this.f.niveles.value) {
      const index = nivelesSelected.indexOf(this.f.niveles.value.maeDetalleId);
      nivelesSelected.splice(index, 1);
      return this.helperService.niveles.filter(
        (b) => !nivelesSelected.includes(b.maeDetalleId)
      );
    } else {
      return this.helperService.niveles.filter(
        (b) => !nivelesSelected.includes(b.maeDetalleId)
      );
    }
  }

  validatePercentage() {
    const value = Number(this.f.porcentaje.value);
    if (value > 100) {
      this.f.porcentaje.patchValue('100');
    } else if (this.f.porcentaje.value.length > 0) {
      this.f.porcentaje.patchValue(value);
    }
  }

  saveBonificacion() {
    this.modalNuevoDetalleForm.markAllAsTouched();
    if (this.modalNuevoDetalleForm.valid) {
      this.onNoClick(this.modalNuevoDetalleForm.getRawValue());
    }
  }
}
