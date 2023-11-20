import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatDialog } from '@angular/material/dialog';
import {
  FormGroup,
  FormControl,
  Validators,
  ValidationErrors,
} from '@angular/forms';
import { HelperPerfilesService } from '../helperPerfiles.service';
import { Const } from '../../../../@data/services/const';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { PerfilesService } from '../../../../@data/services/perfiles.service';
import { RegistroMasivoErrorModalComponent } from '../registro-masivo-error-modal/registro-masivo-error-modal.component';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-registro-masivo-modal',
  templateUrl: './registro-masivo-modal.component.html',
  styleUrls: ['./registro-masivo-modal.component.scss'],
})
export class RegistroMasivoModalComponent implements OnInit {
  resultados: boolean = false;
  modalidades: any[] = [];
  form: FormGroup;
  filename: string = null;
  filesAccept: string;
  file: any = null;
  reg_correctos: number = 0;
  reg_incorrectos: number = 0;
  upload_errors: string[] = [];

  constructor(
    private dialogRef: MatDialogRef<RegistroMasivoModalComponent>,
    public helperPerfilesService: HelperPerfilesService,
    private perfilesService: PerfilesService,
    private toastService: ToastService,
    private dialog: MatDialog,
    private authenticationRepository: AuthenticationRepository,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    this.filesAccept = Const.TIPO_FILES_PERF_REG_MAS.join(',');

    this.form = new FormGroup({
      codProg: new FormControl('', [Validators.required]),
    });

    this.helperPerfilesService.loadCombox();
  }

  get f() {
    return this.form.controls;
  }

  onNoClick() {
    this.dialogRef.close(0);
  }

  changeListener(event: any) {
    if (event.target.files.length > 0) {
      if (this.validFileFype(event.target.files[0].type)) {
        if (event.target.files[0].size / (1024 * 1024) > 1) {
          this.toastService.showToast(
            'El archivo excede el tamaño de 1MB',
            'danger'
          );
          return;
        }

        this.resultados = false;
        this.file = event.target.files[0];
      } else {
        this.toastService.showToast(
          'Seleccione algun archivo adjunto valido',
          'danger'
        );
      }
    } else {
      this.toastService.showToast(
        'Debe seleccionar al menos un archivo para subir',
        'danger'
      );
    }
  }

  uploadFile() {
    let formData = new FormData();
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    formData.append(
      'model',
      JSON.stringify({
        codRegimen: this.form.getRawValue().codProg,
        idEntidad: entidadId,
      })
    );
    formData.append('files', this.file, this.file.name);

    this.perfilesService
      .uploadPlantilla(formData)
      .toPromise()
      .then((res: any) => {
        this.resultados = true;
        this.upload_errors = res.errors;
        this.reg_correctos = res.correctos;
        this.reg_incorrectos = res.incorrectos;
      })
      .catch((error: any) => {
        this.toastService.showToast(JSON.stringify(error), 'danger');
      });
  }

  validFileFype(type: string) {
    return Const.TIPO_FILES_PERF_REG_MAS.indexOf(type) > -1;
  }

  descargarPlantilla() {
    if (this.form.invalid) {
      this.toastService.showToast(
        'Seleccione un regimen antes de descargar una plantilla',
        'danger'
      );
      return;
    }

    let regimen: string;
    switch (this.form.getRawValue().codProg) {
      case Const.MD_DL30057:
        regimen = '30057';
        break;
      case Const.MD_DL1041:
        regimen = 'practicas';
        break;
      default:
        regimen = 'otros';
        break;
    }

    this.perfilesService
      .getPlantillaRegimen(regimen)
      .toPromise()
      .then((base64: any) => {
        this.downloadFile(regimen, base64);
      })
      .catch((error: any) => {
        this.toastService.showToast(JSON.stringify(error), 'danger');
      });
  }

  validInput(event: any) {
    if (this.form.invalid) {
      this.toastService.showToast(
        'Seleccione un regimen antes de subir una plantilla',
        'danger'
      );
      return;
    }

    event.click();
  }

  downloadFile(regimen: string, base64: any) {
    const linkSource = `data:application/vnd.ms-excel.sheet.macroEnabled.12;base64,${base64}`;
    const downloadLink = document.createElement('a');
    const fileName = `plantilla-${regimen}.xlsm`;

    downloadLink.href = linkSource;
    downloadLink.download = fileName;
    downloadLink.click();
  }

  validUploadFile() {
    if (this.form.invalid) {
      this.toastService.showToast(
        'Seleccione un regimen antes de subir una plantilla',
        'danger'
      );
      return;
    }

    if (this.file === null) {
      this.toastService.showToast(
        'Debe seleccionar al menos un archivo antes de subir una plantilla',
        'danger'
      );
      return;
    }

    this.uploadFile();
  }

  subirFinalizarButton() {
    if (this.resultados) {
      this.dialogRef.close(this.reg_correctos);
    } else {
      this.validUploadFile();
    }
  }

  modalError() {
    const dialogReg = this.dialog.open(RegistroMasivoErrorModalComponent, {
      maxHeight: '90vh',
      data: {
        errors: this.upload_errors,
      },
    });
  }
}
