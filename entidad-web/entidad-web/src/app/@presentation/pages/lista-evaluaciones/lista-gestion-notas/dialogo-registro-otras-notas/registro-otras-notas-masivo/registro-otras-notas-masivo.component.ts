import { Component, Input, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import FileSaver from 'file-saver';
import moment from 'moment';
import { ListaGestionOtrasNotasRepository } from 'src/app/@domain/repository/lista-gestion-otras-notas.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64, validateFileExcel } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-registro-otras-notas-masivo',
  templateUrl: './registro-otras-notas-masivo.component.html',
  styleUrls: ['./registro-otras-notas-masivo.component.scss'],
})
export class RegistroOtrasNotasMasivoComponent implements OnInit {
  document: File = null;
  filterForm: FormGroup;
  data: any[][] = [];
  convocatoriaId: string;
  documentBase64: string = '';
  rangePickerStatus: string = 'basic';

  lstPerfilMasivo: any[];
  flagFileChange = false;

  formData: FormData = new FormData();

  @Input() tipoEvaluacion: number;

  constructor(
    private fb: FormBuilder,
    protected ref: MatDialogRef<RegistroOtrasNotasMasivoComponent>,
    private listaGestionOtrasNotasRepository: ListaGestionOtrasNotasRepository,
    private toast: ToastService
  ) {
    this.filterForm = this.fb.group({
      perfilId: new FormControl(null, Validators.required),
      fechaEvaluacion: new FormControl(null, Validators.required),
      observacion: new FormControl(null),
      flagSubirArchivo: new FormControl(false, null),
      documentoBase64: new FormControl(null, Validators.required),
      nombreArchivo: new FormControl(null),
      subirDocumento: [''],
    });
  }

  ngOnInit(): void {
    this.convocatoriaId = sessionStorage.getItem('convocatoriaId');
    this.listarComboPerfil();
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fechaEvaluacion'].value;

    if (this.filterForm.controls['fechaEvaluacion'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  listarComboPerfil() {
    this.listaGestionOtrasNotasRepository
      .getComboPerfil(this.convocatoriaId)
      .subscribe((res) => {
        this.lstPerfilMasivo = res;
      });
  }

  cleanAndClick(inputfile: HTMLInputElement) {
    inputfile.value = '';
    inputfile.click();
  }

  onFileChange(data) {
    this.document = data.target.files[0];
    this.fileChange(data);
  }

  fileChange(event) {
    this.document = null;
    this.documentBase64 = null;
    let result = validateFileExcel(event);

    if (result === 'ok') {
      this.document = event.target.files[0] as File;
      this.flagFileChange = true;
      this.filterForm.get('subirDocumento').setValue(this.document);
      this.filterForm.controls.documentoBase64.setValue(
        "t"
      );
      /** 
            getBase64(this.document).then((data: string) => {
              let base64 = data.split(',');
              if (base64.length > 1) {
                this.documentBase64 = base64[1];
                this.filterForm.controls.documentoBase64.setValue(
                  this.documentBase64
                );
                this.flagFileChange = true;
              }
            });
      */
    } else {
      this.toast.showToast(result, 'danger');
    }
  }

  cleanFileSelect() {
    this.document = null;
    this.filterForm.controls.documentoBase64.setValue('');
    this.documentBase64 = null;
  }

  get f() {
    return this.filterForm.controls;
  }

  guardar() {
    let objSave: any = {
      documentoBase64: this.filterForm.controls.documentoBase64.value,
      fechaEvaluacion: moment(`${this.f.fechaEvaluacion.value}`).format('DD/MM/YYYY'),
      convocatoriaId: this.convocatoriaId,
      perfilId: this.f.perfilId.value,
      observacion: this.f.observacion.value,
      tipoEvaluacion: this.tipoEvaluacion,
    };

    this.formData.append('file', this.filterForm.get('subirDocumento').value);
    this.formData.append("observacion", JSON.stringify(objSave.observacion));
    this.formData.append("fechaEvaluacion", JSON.stringify(objSave.fechaEvaluacion));

    this.listaGestionOtrasNotasRepository
      .guardarCondicionMasivoMtp(objSave, this.formData)
      .subscribe((res) => {
        if (res.status.success) {
          this.toast.showToast("'Carga masiva exitosa'", 'success');
          this.dismiss(true);
        } else {
          let base64 = res.payload.archivo;
          let base64String =
            'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
            base64;
          FileSaver.saveAs(base64String, 'PlantillaCargaMasivaPreguntas');
        }
      });
  }

  downloadTemplate() {
    this.listaGestionOtrasNotasRepository
      .downloadTemplate()
      .subscribe((res) => {
        let base64 = res;
        let base64String =
          'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
          base64;
        FileSaver.saveAs(base64String, 'PlantillaOtrasEvaluaciones');
      });
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }
}
