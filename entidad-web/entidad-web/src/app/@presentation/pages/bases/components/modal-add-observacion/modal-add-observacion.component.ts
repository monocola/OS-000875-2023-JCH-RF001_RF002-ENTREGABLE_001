import { Component, Inject, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ObservacionStepBase } from '../../creacion-base/creacion-base.service';

@Component({
  selector: 'serv-talento-modal-add-observacion',
  templateUrl: './modal-add-observacion.component.html',
  styleUrls: ['./modal-add-observacion.component.scss'],
})
export class ModalAddObservacionComponent implements OnInit {
  observacion = new FormControl('', Validators.required);

  constructor(
    private matDialogRef: MatDialogRef<ModalAddObservacionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ObservacionStepBase
  ) {}

  ngOnInit(): void {
    if (this.data) {
      this.observacion.patchValue(this.data);
    }
  }

  save() {
    this.observacion.markAllAsTouched();
    if (this.observacion.invalid) return;
    this.onNoClick(this.observacion.value);
  }

  onNoClick(value = null) {
    this.matDialogRef.close(value);
  }
}
