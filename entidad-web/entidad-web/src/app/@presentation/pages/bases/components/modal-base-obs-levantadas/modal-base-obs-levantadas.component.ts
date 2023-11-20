import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-base-obs-levantadas',
  templateUrl: './modal-base-obs-levantadas.component.html',
  styleUrls: ['./modal-base-obs-levantadas.component.scss'],
})
export class ModalBaseObsLevantadasComponent implements OnInit {
  constructor(
    private matDialogRef: MatDialogRef<ModalBaseObsLevantadasComponent>
  ) {}

  ngOnInit(): void {}

  onNoClick(value = null) {
    this.matDialogRef.close(value);
  }
}
