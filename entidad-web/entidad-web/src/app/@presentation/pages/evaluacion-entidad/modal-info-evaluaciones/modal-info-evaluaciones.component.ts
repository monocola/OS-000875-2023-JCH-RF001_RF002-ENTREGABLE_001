import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-info-evaluaciones',
  templateUrl: './modal-info-evaluaciones.component.html',
  styleUrls: ['./modal-info-evaluaciones.component.scss'],
})
export class ModalInfoEvaluacionesComponent implements OnInit {
  constructor(
    private dialogRef: MatDialogRef<ModalInfoEvaluacionesComponent>
  ) {}

  ngOnInit(): void {}

  onNoClick() {
    this.dialogRef.close();
  }
}
