import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { MatDialog } from '@angular/material/dialog';
import { FormControl, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'serv-talento-f-dropzone',
  templateUrl: './f-dropzone.component.html',
  styleUrls: ['./f-dropzone.component.scss'],
})
export class FDropzoneComponent implements OnInit {
  // estaSobreElemento = false;

  // ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  // cicloDefaultDesc: string;
  // cicloDefault: number;

  @Input() model;
  @Output() modelChange: EventEmitter<any> = new EventEmitter();
  @Output() afterRemoveFile: EventEmitter<any> = new EventEmitter();
  @Output() upload: EventEmitter<any> = new EventEmitter();
  @Output() downloadPlanilla: EventEmitter<any> = new EventEmitter();
  @Input() allowedFiles: string[];
  @Input() maxSize = 0;

  estaSobreElemento = false;
  errorsFromFile: any[] = [];

  fileOrgano: File = null;
  fileOrganoBase64 = '';

  public fileDataModel = null;
  // public previewUrl = null;
  public errorMsg = '';

  form: FormGroup;

  constructor(
    private toastService: ToastService,
    // private servidoresRepository: ServidoresRepository,
    public dialog: MatDialog
  ) {}

  ngOnInit() {
    this.form = new FormGroup({
      file: new FormControl('', [Validators.required]),
    });
  }

  get fileName() {
    return this.model ? this.model.name : null;
  }

  // setCiclo() {
  //   if (this.ciclo.length !== 0 && this.ciclo) {
  //     this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
  //     this.cicloDefault = this.ciclo.cicloId;
  //   } else {
  //     this.cicloDefaultDesc = '';
  //   }
  // }
  fileChangeEvent(fileInput) {
    this.errorsFromFile = [];
    if (!fileInput.target.files[0]) {
      this.toastService.showToast(
        'Debe ingresar un archivo especificado.',
        'danger'
      );
    }
    if (fileInput.target.files[0]?.size / (1024 * 1024) > 10) {
      // 2048 * 2048
      this.toastService.showToast(
        'El archivo excede el tamaño de 10MB',
        'danger'
      );
    } else {
      const extension = fileInput.target.files[0]?.name.split('.')[
        fileInput.target.files[0].name.split('.').length - 1
      ];
      const extensionesPermitidas = ['xlsm', 'XLSM'];
      console.log(extensionesPermitidas, 'extenciones permitidas');
      if (extensionesPermitidas.includes(extension)) {
        this.fileOrgano = fileInput.target.files[0] as File;
        getBase64(this.fileOrgano).then((data: string) => {
          this.fileOrganoBase64 = data;
        });
      } else {
        this.fileOrgano = null;
        this.fileOrganoBase64 = '';
        this.toastService.showToast(
          'Solo están permitidos los archivos xls y xlsx.',
          'danger'
        );
      }
    }
  }

  removeFile() {
    this.form.patchValue({ file: '' });
    this.fileDataModel = null;
    this.fileOrgano = null;
  }

  uploadFile() {
    // this.upload.emit(this.fileOrganoBase64);
    this.upload.emit(this.fileOrgano);
  }

  // uploadFile() {
  //   this.servidoresRepository
  //     .uploadFileMasivo(this.fileOrgano, this.ciclo.cicloId)
  //     .subscribe( (response: any) => {
  //       console.log(this.fileOrgano);
  //       if (response.type === HttpEventType.UploadProgress) {
  //         this.conteoupload = Math.round(
  //           (100 * response.loaded) / response.total
  //         );
  //       } else if (response instanceof HttpResponse) {
  //         if ( !response.body.status.success ) {
  //           this.toastService.showToast(response.body.status.error.messages[0], 'danger');
  //         } else {
  //           this.toastService.showToast('Se registro la resolución con Exito', 'success', 'Atención');
  //         }
  //       }
  //   });
  // }

  // openDialog(archivo: any[]) {
  //   this.dialog.open(ModalConfirmacionComponent, {
  //     disableClose: true,
  //     data: archivo,
  //   });
  // }

  // openDialogOk() {
  //   this.dialog.open(ModalConfirmacionTodoBienComponent, {
  //     disableClose: true,
  //     data: this.fileOrganoBase64,
  //   });
  // }

  deleteFile() {
    this.form.patchValue({ file: '' });
  }

  downloadFile() {
    this.downloadPlanilla.emit(null);
  }
}
