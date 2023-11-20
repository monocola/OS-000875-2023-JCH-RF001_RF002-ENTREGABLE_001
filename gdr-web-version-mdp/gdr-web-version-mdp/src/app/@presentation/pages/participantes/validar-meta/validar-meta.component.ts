import { Component, Inject, Input, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

import { MetaParticipante } from 'src/app/@data/model/meta';
import { MatTableDataSource } from '@angular/material/table';


const ELEMENT_DATA: MetaParticipante[] = [];


@Component({
  selector: 'serv-talento-validar-meta',
  templateUrl: './validar-meta.component.html',
  styleUrls: ['./validar-meta.component.scss']
})
export class ValidarMetaComponent implements OnInit {
  @Input() title = 'Validar meta';
  @Input() bodyText = '¿Está seguro que desea continuar?';
  @Input() rutaImagen: string = 'assets/images/question.png';
  dataSource = new MatTableDataSource<MetaParticipante>(ELEMENT_DATA);


  constructor(
    private matDialogRef: MatDialogRef<ValidarMetaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private servidoresRepository: ServidoresRepository,
    private toastService: ToastService,


  ) { }

  ngOnInit(): void {
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }

/*Dedo bien */
  validarMetas() {
let body = this.data.body;
console.info(body);
     this.servidoresRepository.validarMeta(body).subscribe(
      (res) => {
       console.log('%cvalidar-meta.component.ts line:46 res', 'color: red;', res);
       if (res.status) {
         this.toastService.showToast(
           'Se realizó la validación de la meta exitosamente',
           'success'
         );
         this.onNoClick(true);
       } else {
         this.toastService.showToast(
           'error',
           'danger'
         );
       }
     },
     (err) => this.toastService.showToast(err.message, 'danger')
   ); 
  }

}



export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
  rutaImagen: string;
  textCancel: string;
  textOk: string;
}

