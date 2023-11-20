import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-modal-notificacion',
  templateUrl: './modal-notificacion.component.html',
  styleUrls: ['./modal-notificacion.component.scss'],
})
export class ModalNotificacionComponent implements OnInit {
  const = Const;
  cantidad = 0;
  coordinador = this.const.R_COORDINADOR;
  estadoRevisar = this.const.ETA_BASE_POR_REVISAR;

  @Input() rutaImagen: string = 'assets/images/icons/revisado.png';

  constructor(
    private matDialogRef: MatDialogRef<ModalNotificacionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    this.cantidad = this.data;
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }
}
