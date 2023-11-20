import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import * as FileSaver from 'file-saver';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { DialogRegistroNotasEvalComponent } from '../dialog-registro-notas-eval.component';
import { ListaGestionNotasRepository } from 'src/app/@domain/repository/lista-gestion-notas.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import {
  CboGrupo,
  CboPerfil,
  NotaMasiva,
} from 'src/app/@data/model/lista-evaluaciones/entity';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { getBase64, validateFileExcel } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-registro-notas-masivo',
  templateUrl: './registro-notas-masivo.component.html',
  styleUrls: ['./registro-notas-masivo.component.scss'],
})
export class RegistroNotasMasivoComponent implements OnInit {
  document: File = null;
  filterForm: FormGroup;
  data: any[][] = [];
  convocatoriaId: string;
  documentBase64: string = '';
  lstGrupoM: CboGrupo[];
  lstPerfilMasivo: CboPerfil[];
  flagFileChange = false;
  uploadForm: FormGroup;

  filteredOptionsGrupoM$: Observable<CboGrupo[]>;
  @ViewChild('inputGrupoM') inputGrupoM: ElementRef;
  optionsGrupoM: CboGrupo[] = [];
  grupoM: CboGrupo[];

  constructor(
    private fb: FormBuilder,
    protected ref: MatDialogRef<DialogRegistroNotasEvalComponent>,
    private listaGestionNotasRepository: ListaGestionNotasRepository,
    private toast: ToastService,
    private formBuilder: FormBuilder,
  ) {
    this.filterForm = this.fb.group({
      perfilId: new FormControl(null, Validators.required),

      programacionIdM: new FormControl(null, Validators.required),
      desGrupoM: new FormControl(null),

      observacion: new FormControl(null),

      flagSubirArchivo: new FormControl(false, null),
      documentoBase64: new FormControl(null, Validators.required),
      nombreArchivo: new FormControl(null),
    });
  }

  ngOnInit(): void {
    this.convocatoriaId = sessionStorage.getItem('convocatoriaId');
    this.uploadForm = this.formBuilder.group({
      subirPlantilla: ['']
    });
    this.listarComboEvaluaciones();
  }

  listarComboEvaluaciones() {
    this.listaGestionNotasRepository
      .getComboEvaluaciones(this.convocatoriaId)
      .subscribe((res) => {
        this.lstPerfilMasivo = res;
      });
  }

  cleanAndClick(inputfile: HTMLInputElement) {
    inputfile.value = '';
    inputfile.click();
  }

  /*** */
  clickAndClean(inputfile: HTMLInputElement) {
    inputfile.value = '';
    inputfile.click();
  }

  onSubirPlantilla(event) {
    this.document = event.target.files[0];
    this.fileChange(event);
    if (event.target.files.length > 0) {
      const file = event.target.files[0];
      this.uploadForm.get('subirPlantilla').setValue(file);
    }
  }
  /*** */

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
    } else {
      this.toast.showToast(result, 'danger');
    }
  }

  cleanFileSelect() {
    this.document = null;
    this.filterForm.controls.documentoBase64.setValue('');
    this.documentBase64 = null;
  }

  guardar() {

    if (this.document != null) {

      let programacionId = +this.filterForm.controls.programacionIdM.value;
      const formData = new FormData();
      formData.append('file', this.uploadForm.get('subirPlantilla').value);
      // console.log(formData);


      this.listaGestionNotasRepository
        .saveNotaExamenMasivo(programacionId, formData)
        .subscribe((res) => {

          if (res.payload.listaErrores.length > 0) {
            let base64 = res.payload.archivo;
            let base64String =
              'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
              base64;
            FileSaver.saveAs(base64String, 'PlantillaCargaMasivaPreguntas');
          } else {
            this.toast.showToast("'Carga masiva exitosa'", 'success');
            this.dismiss(true);
          }
        });


    }





  }

  downloadTemplate() {
    let programacionId = +this.filterForm.controls.programacionIdM.value;

    if (programacionId) {
      this.listaGestionNotasRepository
        .downloadTemplate(programacionId)
        .subscribe((res) => {
          let base64 = res;
          let base64String =
            'data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,' +
            base64;
          FileSaver.saveAs(base64String, 'PlantillaPostulantes');
        });
    } else {
      this.toast.showToast("'Debe seleccionar una programacion'", 'danger');
    }
  }

  cambioPerfil(idPerfil: number) {
    this.cleanCombos();

    if (idPerfil) {
      this.listaGestionNotasRepository
        .getComboGrupos(this.convocatoriaId, idPerfil)
        .subscribe((res) => {
          this.lstGrupoM = res;
          this.filteredOptionsGrupoM$ = of(res);
          this.optionsGrupoM = this.lstGrupoM;
        });
    }
  }

  cleanCombos() {
    this.lstGrupoM = [];
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  private filterGrupoM(value: string): CboGrupo[] {
    const filterValue = value?.toLowerCase();
    return this.optionsGrupoM?.filter((optionValue) =>
      optionValue.desGrupo == null
        ? ''
        : optionValue.desGrupo.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsGrupoM(value: string): Observable<CboGrupo[]> {
    return of(value).pipe(
      map((filterString) => this.filterGrupoM(filterString))
    );
  }

  onChangeGrupoM() {
    this.filteredOptionsGrupoM$ = this.getFilteredOptionsGrupoM(
      this.inputGrupoM.nativeElement.value
    );

    this.onSelectionChangeGrupoM(this.inputGrupoM.nativeElement.value);
  }

  onSelectionChangeGrupoM($event) {
    this.setGrupoM($event);
    this.filteredOptionsGrupoM$ = this.getFilteredOptionsGrupoM($event);
  }

  setGrupoM(item: string) {
    let lstGrupo = this.lstGrupoM?.filter((o) => o.desGrupo === item)[0]
      ?.idProgramacion;
    this.filterForm.get('programacionIdM').setValue(lstGrupo);
  }

  setGrupoByIdM(item: number) {
    let grupo = this.lstGrupoM?.filter((o) => o.idProgramacion === item)[0]
      ?.idProgramacion;
    this.filterForm.get('programacionIdM').setValue(grupo);
  }
}
