import { Component, ElementRef, Input, OnInit, ViewChild } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import moment from 'moment';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import {
  CboOtrosNombre,
} from 'src/app/@data/model/lista-evaluaciones/entity';
import { ListaGestionOtrasNotasRepository } from 'src/app/@domain/repository/lista-gestion-otras-notas.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64, validateFile } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-registro-otras-notas-individual',
  templateUrl: './registro-otras-notas-individual.component.html',
  styleUrls: ['./registro-otras-notas-individual.component.scss'],
})
export class RegistroOtrasNotasIndividualComponent implements OnInit {
  filterForm: FormGroup;
  convocatoriaId: string;
  lstPerfil: any[];
  lstEstado: any[];
  lstNombres: CboOtrosNombre[];
  document: File = null;
  documentBase64: string = '';
  flagFileChange = false;
  rangePickerStatus: string = 'basic';

  filteredOptionsPostulante$: Observable<CboOtrosNombre[]>;
  @ViewChild('inputPostulante') inputPostulante: ElementRef;
  optionsPostulante: CboOtrosNombre[] = [];
  formData = new FormData();
  uploadForm: FormGroup;

  @Input() tipoEvaluacion: number;

  constructor(
    private fb: FormBuilder,
    private listaGestionOtrasNotasRepository: ListaGestionOtrasNotasRepository,
    private toast: ToastService,
    protected ref: MatDialogRef<RegistroOtrasNotasIndividualComponent>,
    private formBuilder: FormBuilder,
  ) {
    this.filterForm = this.fb.group({
      perfilId: new FormControl(null),
      fechaEvaluacion: new FormControl(null, Validators.required),
      flagApto: new FormControl(null, Validators.required),

      observacion: new FormControl(null),
      postulanteId: new FormControl(null, Validators.required),
      desPostulante: new FormControl(null),

      flagSubirArchivo: new FormControl(false, null),
      documentoBase64: new FormControl(null),
      nombreArchivo: new FormControl(null)
    });
  }

  ngOnInit(): void {
    this.convocatoriaId = sessionStorage.getItem('convocatoriaId');
    this.listarComboPerfil();
    this.listaEstadoEvaluacion();

    this.uploadForm = this.formBuilder.group({
      subirPlantilla: ['']
    });
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fechaEvaluacion'].value;

    if (this.filterForm.controls['fechaEvaluacion'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  onFileChange(data) {
    this.document = data.target.files[0];
    this.fileChange(data);
  }

  cleanFileSelect() {
    this.document = null;
  }

  fileChange(event) {
    this.document = null;
    this.documentBase64 = null;
    let result = validateFile(event);

    if (result === 'ok') {
      this.document = event.target.files[0] as File;
      this.uploadForm.get('subirPlantilla').setValue(this.document);

      this.flagFileChange = true;
      /**
      getBase64(this.document).then((data: string) => {
        let base64 = data.split(',');
        if (base64.length > 1) {
          this.documentBase64 = base64[1];
          this.flagFileChange = true;
        }
      });
       */
    } else {
      this.toast.showToast(result, 'danger');
    }
  }

  listarComboPerfil() {
    this.listaGestionOtrasNotasRepository
      .getComboPerfil(this.convocatoriaId)
      .subscribe((res) => {
        this.lstPerfil = res;
      });
  }

  listaEstadoEvaluacion() {
    let flag = '';

    if (this.tipoEvaluacion === 1) {
      flag = 'ESTADO_CONOCIMIENTO';
    } else if (this.tipoEvaluacion === 2) {
      flag = 'ESTADO_PSICOMETRICO';
    } else if (this.tipoEvaluacion === 3) {
      flag = 'ESTADO_PSICOLOGICA';
    } else if (this.tipoEvaluacion === 4) {
      flag = 'ESTADO_COMPETENCIAS';
    } else if (this.tipoEvaluacion === 5) {
      flag = 'ESTADO_CURRICULAR';
    } else if (this.tipoEvaluacion === 6) {
      flag = 'ESTADO_ENTREVISTA';
    }

    this.listaGestionOtrasNotasRepository
      .listaEstadoEvaluacion(flag)
      .subscribe((res) => {
        this.lstEstado = res;
      });
  }

  listarComboPostulantes(perfilId: number) {
    this.listaGestionOtrasNotasRepository
      .getComboPostulantes(this.convocatoriaId, perfilId)
      .subscribe((res) => {
        this.lstNombres = res;
        this.filteredOptionsPostulante$ = of(res);
        this.optionsPostulante = this.lstNombres;
      });
  }

  cleanAndClick(inputfile: HTMLInputElement) {
    inputfile.value = '';
    inputfile.click();
    this.uploadForm.get('subirPlantilla').setValue(null);
  }

  cambioPerfil(perfilId: number) {
    this.listarComboPostulantes(perfilId);
  }

  onChangePostulante() {
    this.filteredOptionsPostulante$ = this.getFilteredOptionsPostulante(
      this.inputPostulante.nativeElement.value
    );

    this.onSelectionChangePostulante(this.inputPostulante.nativeElement.value);
  }

  onSelectionChangePostulante($event) {
    this.setPostulante($event);
    this.filteredOptionsPostulante$ = this.getFilteredOptionsPostulante($event);
  }

  setPostulante(item: string) {
    let lstPostulante = this.lstNombres?.filter(
      (o) => o.nombreCompleto === item
    )[0]?.idPostulante;
    this.filterForm.get('postulanteId').setValue(lstPostulante);
  }

  getFilteredOptionsPostulante(value: string): Observable<CboOtrosNombre[]> {
    return of(value).pipe(
      map((filterString) => this.filterPostulante(filterString))
    );
  }

  private filterPostulante(value: string): CboOtrosNombre[] {
    const filterValue = value?.toLowerCase();
    return this.optionsPostulante?.filter((optionValue) =>
      optionValue.nombreCompleto == null
        ? ''
        : optionValue.nombreCompleto.toLowerCase().includes(filterValue)
    );
  }

  guardar() {
    let individual = this.filterForm.value;
    individual.flagApto = +individual.flagApto;
    individual.tipoEvaluacion = this.tipoEvaluacion;
    individual.flagSubirArchivo = this.flagFileChange;
    individual.nombreArchivo = this.document?.name;
    individual.documentoBase64 = this.documentBase64;

    individual.fechaEvaluacion = moment(individual.fechaEvaluacion).format('DD/MM/YYYY');

    let payload = { otrasEvaluaciones: individual };
    this.formData.append('file', this.uploadForm.get('subirPlantilla').value);
    this.formData.append('json', JSON.stringify(payload));
    console.info(payload);
    this.listaGestionOtrasNotasRepository
      .guardarOtraNotaIndividualMtp(this.formData, individual.perfilId, +this.convocatoriaId)
      .subscribe(
        (res) => {
          this.toast.showToast(res, 'success');
          this.limpiarFormulario();
        },
        (err) => {
          this.toast.showToast(err, 'warning');
        }
      );
  }

  limpiarFormulario() {
    this.filterForm.reset();
    this.document = null;
    this.lstNombres = [];
    this.filterForm.controls.desPostulante.setValue('');
    this.filteredOptionsPostulante$ = null;
    this.formData = new FormData();
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }
}
