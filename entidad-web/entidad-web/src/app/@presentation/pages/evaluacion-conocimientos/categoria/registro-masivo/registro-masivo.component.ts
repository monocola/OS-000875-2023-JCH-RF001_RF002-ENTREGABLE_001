import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import * as FileSaver from 'file-saver';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { HttpEventType, HttpResponse } from '@angular/common/http';
import { FormGroup, FormBuilder } from '@angular/forms';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { of } from 'rxjs';
import { validateFileExcel } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-registro-masivo',
  templateUrl: './registro-masivo.component.html',
  styleUrls: ['./registro-masivo.component.scss'],
})
export class RegistroMasivoComponent implements OnInit {
  categoria = null;
  idcategoria = 0;

  document: File = null;
  uploadForm: FormGroup;

  showResumen: boolean = false;
  showEnableGuardar: boolean = false;
  objValidacionPlantilla: any;

  flag: boolean = false;
  archivoErrores;

  constructor(
    private matDialogRef: MatDialogRef<RegistroMasivoComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    protected ref: MatDialogRef<RegistroMasivoComponent>,
    private toastService: ToastService,
    private formBuilder: FormBuilder,

  ) {}

  ngOnInit(): void {
    this.categoria = this.data.nombreCategoria;
    this.idcategoria = this.data.idCategoria;
    this.uploadForm = this.formBuilder.group({
      subirPlantilla: ['']
    });
  }

  onNoClick(type: boolean = false) {
    this.matDialogRef.close(type);
  }

  cleanFileSelect() {
    this.document = null;

    this.showResumen = false;
 }

  clickAndClean(inputfile: HTMLInputElement) {
    inputfile.value = '';
    inputfile.click();
 }

 fileChange(event) {
  this.document = null;
  let result = validateFileExcel(event);
  if (result === 'ok') {
    this.document = event.target.files[0] as File;
  } else {
    this.toastService.showToast(result, 'danger');
  }
}
dismiss(success: boolean) {
  this.ref.close(success);
}

  onClickDownloadExcel() {
    this.evaluacionConocimientosService
      .downloadTemplateExcel(this.categoria)
      .subscribe((res) => {
        let base64String =
          'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
          res;
        FileSaver.saveAs(base64String, 'PlantillaCargaMasivaPreguntas');
      });
  }

  onSubirPlantilla(event) {

    this.showResumen = true;
    console.log(this.idcategoria);
    this.document = event.target.files[0];
    this.fileChange(event);

    if (event.target.files.length > 0) {
      const file = event.target.files[0];
      this.uploadForm.get('subirPlantilla').setValue(file);
      const formData = new FormData();
      formData.append('file', this.uploadForm.get('subirPlantilla').value);
      this.evaluacionConocimientosService.validaPlantillaPreguntasRespuestas(formData,this.idcategoria).subscribe((res) => {
        this.objValidacionPlantilla = res;
        console.log(res);

        if (res.totalIncorrectos === 0 && res.totalRegistros > 0 && res.totalDuplicados === 0 && res.totalCorrectos > 1) {
          this.showEnableGuardar = true;
        }

      });
    }
  }

  descargarArchivoErrores(file) {
    let base64 = file;
    let base64String =
      'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
      base64;
    FileSaver.saveAs(base64String, 'ArchivoErrores');
  }
  guardarPreguntas() {
    if (this.objValidacionPlantilla.listaCorrecta.length > 0) {
      this.evaluacionConocimientosService.guardarExamenPreguntasRespuestasMasiva(this.idcategoria,this.objValidacionPlantilla.listaCorrecta).subscribe((res) => {
        this.toastService.showToast('Registro exitoso','success','Éxito');
        this.dismiss(true);
      });
    } else {
      this.toastService.showToast('Archivo adjunto con errores','danger','Atención');
    }
  }

}

export interface ModalConfirmationModel {
  title: string;
  bodyText: string;
}
