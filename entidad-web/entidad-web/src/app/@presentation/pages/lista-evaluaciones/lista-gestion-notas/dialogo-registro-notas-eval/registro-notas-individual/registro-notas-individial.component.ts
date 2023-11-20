import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import {
  CboGrupo,
  CboNombre,
  CboPerfil,
} from 'src/app/@data/model/lista-evaluaciones/entity';
import { ListaGestionNotasRepository } from 'src/app/@domain/repository/lista-gestion-notas.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64, validateFile } from 'src/app/utils/converterFile';
import { CurrencyPipe } from '@angular/common';
import { MatDialogRef } from '@angular/material/dialog';
import { DialogRegistroNotasEvalComponent } from '../dialog-registro-notas-eval.component';

@Component({
  selector: 'serv-talento-registro-notas-individial',
  templateUrl: './registro-notas-individial.component.html',
  styleUrls: ['./registro-notas-individial.component.scss'],
})
export class RegistroNotasIndividialComponent implements OnInit {
  lstPerfil: CboPerfil[];
  lstGrupo: CboGrupo[];
  lstNombres: CboNombre[];
  convocatoriaId: string;
  filterForm: FormGroup;
  document: File = null;
  documentBase64: string = '';
  flagFileChange = false;
  numeroDocumentoType: string = 'integer';

  filteredOptionsGrupo$: Observable<CboGrupo[]>;
  @ViewChild('inputGrupo') inputGrupo: ElementRef;
  optionsGrupo: CboGrupo[] = [];
  grupo: CboGrupo[];

  filteredOptionsPostulante$: Observable<CboNombre[]>;
  @ViewChild('inputPostulante') inputPostulante: ElementRef;
  optionsPostulante: CboNombre[] = [];
  postulante: CboNombre[];

  constructor(
    private listaGestionNotasRepository: ListaGestionNotasRepository,
    protected ref: MatDialogRef<DialogRegistroNotasEvalComponent>,
    private toast: ToastService,
    private currencyPipe: CurrencyPipe,
    private fb: FormBuilder
  ) {
    this.filterForm = this.fb.group({
      perfilId: new FormControl(null, Validators.required),
      situacionId: new FormControl(null, Validators.required),
      puntaje: new FormControl(null, Validators.required),
      codigo: new FormControl(null, Validators.required),
      observacion: new FormControl(null),

      programacionId: new FormControl(null, Validators.required),
      desGrupo: new FormControl(null),

      postulanteId: new FormControl(null, Validators.required),
      desPostulante: new FormControl(null),

      flagSubirArchivo: new FormControl(false, null),
      documentoBase64: new FormControl(null),
      nombreArchivo: new FormControl(null),
    });
  }

  ngOnInit(): void {
    this.convocatoriaId = sessionStorage.getItem('convocatoriaId');
    this.listarComboEvaluaciones();
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
    let result = validateFile(event);

    if (result === 'ok') {
      this.document = event.target.files[0] as File;
      getBase64(this.document).then((data: string) => {
        let base64 = data.split(',');
        if (base64.length > 1) {
          this.documentBase64 = base64[1];
          this.flagFileChange = true;
        }
      });
    } else {
      this.toast.showToast(result, 'danger');
    }
  }

  cleanFileSelect() {
    this.document = null;
  }

  listarComboEvaluaciones() {

    this.listaGestionNotasRepository
      .getComboEvaluaciones(this.convocatoriaId)
      .subscribe((res) => {
        this.lstPerfil = res;
      });
  }

  listarComboPostulantes(programacionId: number) {
    this.listaGestionNotasRepository
      .getComboPostulantes(programacionId)
      .subscribe((res) => {
        this.lstNombres = res;
        this.filteredOptionsPostulante$ = of(res);
        this.optionsPostulante = this.lstNombres;
      });
  }

  cambioPerfil(idPerfil: number) {
    this.cleanCombos();

    if (idPerfil) {
      this.listaGestionNotasRepository
        .getComboGrupos(this.convocatoriaId, idPerfil)
        .subscribe((res) => {
          this.lstGrupo = res;
          this.filteredOptionsGrupo$ = of(res);
          this.optionsGrupo = this.lstGrupo;
        });
    }
  }

  cleanCombos() {
    this.lstGrupo = [];
    this.filterForm.controls.programacionId.setValue(null);
    this.filterForm.controls.desGrupo.setValue('');
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  guardar() {
    let individual = this.filterForm.value;
    individual.puntaje = +individual.puntaje;

    individual.flagSubirArchivo = this.flagFileChange;
    individual.nombreArchivo = this.document?.name;
    individual.documentoBase64 = this.documentBase64;

    this.listaGestionNotasRepository
      .guardarNotaExamenIndividual(individual)
      .subscribe(
        (res) => {
          console.info(res);
        if(res.status.success) {
           this.toast.showToast('Registro guardado exitosamente', 'success');
           this.limpiarFormulario();
           this.dismiss(true);
        } else {
          this.toast.showToast(res.status.error.messages[0], 'warning');
        }
      },
      (error) => {
        this.toast.showToast(error.message, 'warning');
      }
      );
  }

  limpiarFormulario() {
    this.filterForm.reset();
    this.document = null;
    this.lstGrupo = [];
    this.lstNombres = [];
    this.filterForm.controls.desGrupo.setValue('');
    this.filterForm.controls.desPostulante.setValue('');
    this.filteredOptionsGrupo$ = null;
    this.filteredOptionsPostulante$ = null;
  }

  private filterGrupo(value: string): CboGrupo[] {
    const filterValue = value?.toLowerCase();
    return this.optionsGrupo?.filter((optionValue) =>
      optionValue.desGrupo == null
        ? ''
        : optionValue.desGrupo.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsGrupo(value: string): Observable<CboGrupo[]> {
    return of(value).pipe(
      map((filterString) => this.filterGrupo(filterString))
    );
  }

  onChangeGrupo() {
    this.filteredOptionsGrupo$ = this.getFilteredOptionsGrupo(
      this.inputGrupo.nativeElement.value
    );

    this.onSelectionChangeGrupo(this.inputGrupo.nativeElement.value);
  }

  onSelectionChangeGrupo($event) {
    this.setGrupo($event);
    this.filteredOptionsGrupo$ = this.getFilteredOptionsGrupo($event);
  }

  setGrupo(item: string) {
    let lstGrupo = this.lstGrupo?.filter((o) => o.desGrupo === item)[0]
      ?.idProgramacion;
    this.filterForm.get('programacionId').setValue(lstGrupo);

    if (lstGrupo) {
      this.listarComboPostulantes(lstGrupo);
    }
  }

  setGrupoById(item: number) {
    let grupo = this.lstGrupo?.filter((o) => o.idProgramacion === item)[0]
      ?.idProgramacion;
    this.filterForm.get('programacionId').setValue(grupo);
  }

  private filterPostulante(value: string): CboNombre[] {
    const filterValue = value?.toLowerCase();
    return this.optionsPostulante?.filter((optionValue) =>
      optionValue.nombreApellido == null
        ? ''
        : optionValue.nombreApellido.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsPostulante(value: string): Observable<CboNombre[]> {
    return of(value).pipe(
      map((filterString) => this.filterPostulante(filterString))
    );
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
      (o) => o.nombreApellido === item
    )[0]?.postulanteId;
    this.filterForm.get('postulanteId').setValue(lstPostulante);
  }

  setPostulanteById(item: number) {
    let postulante = this.lstNombres?.filter((o) => o.postulanteId === item)[0]
      ?.postulanteId;
    this.filterForm.get('postulanteId').setValue(postulante);
  }

  keyPressNumbers(event) {
    const charCode = event.which ? event.which : event.keyCode;
    if (charCode < 48 || charCode > 57) {
      event.preventDefault();
      return false;
    } else {
      return true;
    }
  }
}
