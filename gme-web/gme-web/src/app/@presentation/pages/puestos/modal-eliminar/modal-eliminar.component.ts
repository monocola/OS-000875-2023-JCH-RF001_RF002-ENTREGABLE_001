import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Router } from '@angular/router';

@Component({
  selector: 'gme-web-modal-eliminar',
  templateUrl: './modal-eliminar.component.html',
  styleUrls: ['./modal-eliminar.component.scss']
})
export class ModalEliminarComponent implements OnInit {
  @Input() title = 'Eliminar';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() link = null;

  constructor(
    private matDialogRef: MatDialogRef<ModalEliminarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel,
    private route: Router
    ) { }

  ngOnInit(): void {
  }

  onNoClick(type: boolean = false) {
    if (!type && this.data.link) {
      this.route.navigate([this.data.link, ]);
    }
    this.matDialogRef.close(type);
  }

}

export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  textCancel: string;
  textOk: string;
  link?: string;
} 
