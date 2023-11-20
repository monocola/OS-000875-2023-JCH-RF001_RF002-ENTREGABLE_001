import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatTableDataSource } from '@angular/material/table';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-btn-habilitar-edicion',
  templateUrl: './modal-btn-habilitar-edicion.component.html',
  styleUrls: ['./modal-btn-habilitar-edicion.component.scss']
})
export class ModalBtnHabilitarEdicionComponent implements OnInit {

  @Input() title = 'Habilitar edición';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/question.png';
  dataSource = new MatTableDataSource();

  constructor(
    private matDialogRef: MatDialogRef<ModalBtnHabilitarEdicionComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toastService: ToastService,
    protected ref: MatDialogRef<ModalBtnHabilitarEdicionComponent>,


  ) { }

  ngOnInit(): void {
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

}


export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}

