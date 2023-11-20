import {
  Component,
  ContentChild,
  Inject,
  OnDestroy,
  OnInit,
} from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { HelperLeyComponentsPerfilesService } from '../helperComponentPerfiles.service';
import cloneDeep from 'lodash/cloneDeep';

@Component({
  selector: 'serv-talento-modal-nivel-educatvo',
  templateUrl: './modal-nivel-educatvo.component.html',
  styleUrls: ['./modal-nivel-educatvo.component.scss'],
})
export class ModalNivelEducatvoComponent implements OnInit, OnDestroy {
  carrerasSelected = [];
  carrera = new FormControl('');
  editMode = false;

  const = Const;

  registerForm: FormGroup = this.fb.group({
    formacionAcademicaId: [null],
    carrerasToDelete: [[]],
    nivel: ['', Validators.required],
    tipoNivel: ['', Validators.required],
    grado: [''],
    tipoGrado: '',
    nombreGrado: '',
    estado: 1,
  });

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalNivelEducatvoComponent>,
    public helperService: HelperLeyComponentsPerfilesService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.helperService.carrerasToShow = this.helperService.carreras.slice(0);
    if (!this.helperService.dataFetched) {
      this.helperService.loadCombox();
    }
    if (this.data) {
      if (this.data.grado) {
        let id = this.data.grado;
        await this.helperService.getCarrerasById(id);
      }
      this.setData(this.data);
      this.editMode = true;
    }

    console.log (this.helperService.nivelesEducativos);
    console.log (this.helperService.grados);
  }

  ngOnDestroy(): void {
    this.editMode = false;
  }

  get f() {
    return this.registerForm.controls;
  }

  setData(data) {
    this.registerForm.patchValue({
      formacionAcademicaId: data.formacionAcademicaId,
      nivel: data.nivel,
      tipoNivel: data.tipoNivel,
      grado: data.grado,
      tipoGrado: data.tipoGrado,
      nombreGrado: data.nombreGrado,
      carrerasToDelete: data.carrerasToDelete,
      estado: 1,
    });

    data.carreras.map((c) => {
      this.carrera.setValue(c);
      this.selectChange();
    });
  }

  remove(index) {
    const idToRemove = this.carrerasSelected[index].id;

    if (this.carrerasSelected[index].carreraFormacionAcademicaId) {
      this.f.carrerasToDelete.patchValue([
        ...this.f.carrerasToDelete.value,
        cloneDeep(this.carrerasSelected[index]),
      ]);
    }
    this.carrerasSelected[index].carreraFormacionAcademicaId = null;
    this.helperService.carrerasToShow.unshift(
      this.helperService.carreras.filter((c) => c.id === idToRemove)[0]
    );
    this.carrerasSelected = this.carrerasSelected.filter(
      (c) => c.id !== idToRemove
    );
  }

  selectChange() {
    if (this.carrera.value) {
      const idCarreraSelected = this.carrera.value.id;
      this.helperService.carrerasToShow = this.helperService.carrerasToShow.filter(
        (c) => c.id !== idCarreraSelected
      );
      const career = this.helperService.carreras.filter(
        (c) => c.id === idCarreraSelected
      )[0];
      career.carreraFormacionAcademicaId =
        this.carrera.value.carreraFormacionAcademicaId || null;
      this.carrerasSelected.push(career);
      this.carrera.patchValue('');
    }
  }

  codeToId(code: string) {
    return (
      this.helperService.grados.filter((g) => g.codProg === code)[0]
        ?.maeDetalleId || false
    );
  }

  captureChange() {
    let listaTemporal = this.carrerasSelected;
    let tam = listaTemporal.length - 1;
    listaTemporal.forEach((e, index) => {
      this.remove(tam - index);
    });
    this.carrera.patchValue('');
    this.carrerasSelected = []; // Limpiamos carreras
    const value = this.f.grado.value;
    if (value) {
      this.helperService.getCarrerasById(value);
    }

    if (
      value === this.codeToId(Const.SIT_ACA_MAE) ||
      value === this.codeToId(Const.SIT_ACA_DOC)
    ) {
      this.registerForm.get('tipoGrado').setValidators(Validators.required);
      this.registerForm.get('nombreGrado').setValidators(Validators.required);
      this.registerForm.get('nombreGrado').updateValueAndValidity();
      this.registerForm.get('tipoGrado').updateValueAndValidity();
    } else {
      this.registerForm.get('tipoGrado').clearValidators();
      this.registerForm.get('nombreGrado').clearValidators();
      this.registerForm.get('tipoGrado').updateValueAndValidity();
      this.registerForm.get('nombreGrado').updateValueAndValidity();
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  saveForm() {
    this.registerForm.markAllAsTouched();
    if (this.registerForm.valid) {
      const newData = {
        ...this.registerForm.getRawValue(),
        carreras: this.carrerasSelected,
        orden: this.data ? this.data.orden : null,
      };

      if (newData.grado === null || newData.grado === undefined || newData.grado === '') {
        const noAplica = this.helperService.grados.find (x => x.codProg === Const.SIT_ACA_NO_APLICA);
        if (noAplica) {
          newData.grado = noAplica.maeDetalleId;
        }
      }

      this.onNoClick(newData);
    }
  }

  nivelChanged(event: any) {
    if (
      event === Const.NVL_EDU_UNI ||
      event === Const.NVL_EDU_TEC_BAS ||
      event === Const.NVL_EDU_TEC_SUP
    ) {
      this.registerForm.controls['grado'].setValidators([Validators.required]);
    } else {
      this.registerForm.controls['grado'].setValidators([]);
    }

    this.registerForm.controls['grado'].updateValueAndValidity();
  }
}
