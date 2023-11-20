
import { Component, OnInit, Input, Output, EventEmitter, Inject  } from '@angular/core';
import { ServidoresRepository } from '../../../../@domain/repository/servidores.repository';
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

   // @Input() downloadFile: boolean = true;
   @Output() closeOrgano = new EventEmitter();
   @Output() updateOrganos = new EventEmitter();

   fileArchivo = null;
   fileOrganoBase64 = null; 
   file: any[];

  constructor(
   private servidoresRepository: ServidoresRepository,
   private toastService: ToastService,
   protected ref: MatDialogRef<ModalConfirmacionComponent>,
   @Inject(MAT_DIALOG_DATA) public data: any[]

  ) {
    // this.fileOrganoBase64 = data;
    this.file = data;
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
