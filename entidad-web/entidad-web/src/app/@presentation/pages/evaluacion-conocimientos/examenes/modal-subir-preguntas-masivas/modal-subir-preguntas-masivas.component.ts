import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Subscription } from 'rxjs';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ToastService } from './../../../../@common-components/toast';
import { validateFileExcel } from 'src/app/utils/converterFile';
import { FormGroup, FormBuilder } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { Component, OnInit } from '@angular/core';
import FileSaver from 'file-saver';
import { CssSelector } from '@angular/compiler';

@Component({
  selector: 'serv-talento-modal-subir-preguntas-masivas',
  templateUrl: './modal-subir-preguntas-masivas.component.html',
  styleUrls: ['./modal-subir-preguntas-masivas.component.scss']
})
export class ModalSubirPreguntasMasivasComponent implements OnInit {

  document: File = null;
  uploadForm: FormGroup;

  showResumen: boolean = false;
  showEnableGuardar: boolean = false;
  objValidacionPlantilla: any;
  entidadId: number;
  constructor(
    private dialogRef: MatDialogRef<ModalSubirPreguntasMasivasComponent>,
    protected ref: MatDialogRef<ModalSubirPreguntasMasivasComponent>,
    private formBuilder: FormBuilder,
    private toast: ToastService,
    private authenticationRepository: AuthenticationRepository,
    private evaluacionConocimientoService: EvaluacionConocimientosRepository,

  ) {
  }

  ngOnInit(): void {
    this.entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;
    this.uploadForm = this.formBuilder.group({
      subirPlantilla: ['']
    });
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
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
      this.toast.showToast(result, 'danger');
    }
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }


  downloadTemplate() {
    this.evaluacionConocimientoService.getPlantillaPreguntasMasivas().subscribe((res) => {
      let base64 = res;
      let base64String =
        'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
        base64;
      FileSaver.saveAs(base64String, 'PlantillaCargaMasivaExamen');
    });
  }

  onSubirPlantilla(event) {

    this.showResumen = true;
    console.log(this.entidadId);
    this.document = event.target.files[0];
    this.fileChange(event);

    if (event.target.files.length > 0) {
      const file = event.target.files[0];
      this.uploadForm.get('subirPlantilla').setValue(file);

      const formData = new FormData();
      formData.append('file', this.uploadForm.get('subirPlantilla').value);
      this.evaluacionConocimientoService.validaPlantillaPreguntas(formData,this.entidadId).subscribe((res) => {
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

  guardar() {
    if (this.objValidacionPlantilla.listaCorrecta.length > 0) {
      this.evaluacionConocimientoService.guardarExamenPreguntasMasiva(this.entidadId,this.objValidacionPlantilla.listaCorrecta).subscribe((res) => {
        this.toast.showToast('Registro exitoso','success','Éxito');
        this.dismiss(true);
      });
    } else {
      this.toast.showToast('Archivo adjunto con errores','danger','Atención');
    }
  }

}
