import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { getBase64 } from 'src/app/utils/converterFile';
import { ToastService } from '../toast';

@Component({
  selector: 'serv-talento-file-dropzone',
  templateUrl: './file-dropzone.component.html',
  styleUrls: ['./file-dropzone.component.scss']
})
export class FileDropzoneComponent implements OnInit {

 
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
  public previewUrl = null;
  public errorMsg = '';

  form: FormGroup;

  constructor(
    private toastService: ToastService,
    public dialog: MatDialog,
  ) { }

  ngOnInit() {
    this.form = new FormGroup({
      file: new FormControl('', [Validators.required])
    });
  }

  get fileName() {
    return this.model ? this.model.name : null;
  }

  fileChangeEvent(fileInput) {
    this.errorsFromFile = [];
    if (!fileInput.target.files[0]) {
      this.toastService.showToast(
        'Debe ingresar un archivo especificado.',
        'danger'
      );
    }
    if (fileInput.target.files[0]?.size / (1024 * 1024) > 0.5) {
      this.toastService.showToast(
        'El archivo excede el tamaño de 500Kb',
        'danger'
      );
    } else {
      const extension = fileInput.target.files[0]?.name.split('.')[
        fileInput.target.files[0].name.split('.').length - 1
      ];
      const extensionesPermitidas = ['xlsx', 'XLSX'];
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
    this.fileDataModel = null;
    this.fileOrgano = null;
  }


  uploadFile() {
    this.upload.emit(this.fileOrganoBase64);
  }

  

  deleteFile() {
    this.form.patchValue({file : ''});
  }
  downloadFile() {
    this.downloadPlanilla.emit(null);
  }
}
