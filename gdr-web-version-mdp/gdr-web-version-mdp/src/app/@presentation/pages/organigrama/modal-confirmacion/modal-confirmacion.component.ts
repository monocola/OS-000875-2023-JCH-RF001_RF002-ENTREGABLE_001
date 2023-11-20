import { Component, OnInit, Input, Output, EventEmitter, Inject  } from '@angular/core';
import { OrganigramaRepository } from '../../../../@domain/repository/organigrama.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { base64ToFilePromise, getBase64 } from 'src/app/utils/converterFile';
import * as FileSaver from 'file-saver';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-confirmacion',
  templateUrl: './modal-confirmacion.component.html',
  styleUrls: ['./modal-confirmacion.component.scss']
})
export class ModalConfirmacionComponent implements OnInit {
   @Output() closeOrgano = new EventEmitter();
   @Output() updateOrganos = new EventEmitter();

   fileArchivo = null;
   file: any[]; 

  constructor(
   private organigramaRepository: OrganigramaRepository,
   private toastService: ToastService,
   protected ref: MatDialogRef<ModalConfirmacionComponent>,
   @Inject(MAT_DIALOG_DATA) public data: any[]

  ) {
    this.file = data;
    console.log('%cmodal-confirmacion.component.ts line:28 data', 'color: red;', data);
  }

  ngOnInit(): void {}

  onClickNo(): void {
    this.ref.close();
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  uploadFile() {
 
            const rutaFileError = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${this.file}`;
            base64ToFilePromise(
              rutaFileError,
              'Archivo con errores.xlsx',
              ''
            ).then((file) => FileSaver.saveAs(file));
  }


}



