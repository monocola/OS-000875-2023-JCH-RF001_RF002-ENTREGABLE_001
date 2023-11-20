import { Component, OnInit, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { AbstractControl, AsyncValidatorFn, FormBuilder, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { OrganigramaRepository } from '../../../../@domain/repository/organigrama.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { UnidadOrganicaRepository } from '../../../../@domain/repository/unidad-organica.repository';
import { forkJoin, Observable } from 'rxjs';
import { UnidadOrganicaCombo } from '../../../../@data/model/unidadOrganicaCombo';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';

import { MatTableDataSource } from '@angular/material/table';
import { map } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-modal-creacion-uo',
  templateUrl: './modal-creacion-uo.component.html',
  styleUrls: ['./modal-creacion-uo.component.scss']
})
export class ModalCreacionUoComponent implements OnInit {
  organos: MaestraParametro[];
  editionMode: boolean = false;
  filterForm: FormGroup;
  mensaje: any;
  sigla: any;
  codigo: any;
  selectOrgano;
  selectTipoOrgano;

  unidadOrganicaCbo: UnidadOrganicaCombo[];
  unidadOrganicaSup: UnidadOrganicaCombo[];
  searchMode = false;
  tableDataSource = new MatTableDataSource([]);

  constructor(
    private matDialog: MatDialogRef<ModalCreacionUoComponent>,
    private fb: FormBuilder,
    private organigramaRepository: OrganigramaRepository,
    private toastService: ToastService,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private UnidadOrganicaRepository: UnidadOrganicaRepository,
    private maeParametroRepository: MaestraParametroRepository,

  ) {}

  ngOnInit(): void {
    this.loadCombox();
    this.initializeForm();
  }

  get f() {
     return this.filterForm.controls;
  }

  initializeForm() {
    if (this.data.createMode ) {
    this.filterForm = this.fb.group({
      estadoRegistro: ['1', [Validators.required]],
      padreOrganigramaId: [''],
      tipoOrganoUoId: 0,
      orden: [''],
      naturalezaOrgano: [''],
      descripcion: ['', [Validators.required, Validators.maxLength(300) ], [this.descripcionValidator]],
      sigla: ['', [Validators.required, Validators.maxLength(10) ], [this.siglaValidator ]],
      tipoOrganoId: ['', [Validators.required]],
      unidadOrganicaSuperiorId: ['', [Validators.required]],
    });
    } else {
      const uo = this.data.dataToEdit;
      this.selectOrgano = uo.uoSuperiorId;
      this.selectTipoOrgano = uo.tipoOrganoId;

      // this.filterForm.patchValue({
        this.filterForm = this.fb.group({
        estadoRegistro: ['1', [Validators.required]],
        padreOrganigramaId: [''],
        tipoOrganoUoId: 0,
        orden: [''],
        naturalezaOrgano: [''],
        descripcion: [uo.nombreUO, [Validators.required, Validators.maxLength(300) ], [this.descripcionValidator]],
        sigla: [uo.siglaUO,  [Validators.required, Validators.maxLength(10) ], [this.siglaValidator ]],
        tipoOrganoId: [uo.tipoOrganoId, [Validators.required]],
        unidadOrganicaSuperiorId: [uo.uoSuperiorId, [Validators.required]],
      });
    }

  }

  loadCombox() {
    this.searchMode = false;
    const getOrganos = this.maeParametroRepository.getMaestraParametro('TIPO_NATURALEZA');
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();
    const getUndOrganicaSup = this.UnidadOrganicaRepository.getUnidadOrganicaSup();
    forkJoin([getOrganos, getUndOrganicaCbo, getUndOrganicaSup, ]).subscribe(
      (results ) => {
        this.organos = results[0];
        this.unidadOrganicaCbo = results[1];
        this.unidadOrganicaSup = results[2];
        console.log(this.unidadOrganicaSup);

      },
      (error) => {}
    );
    console.log(this.unidadOrganicaSup);
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  updateForm() {}

/*   updateForm() {
    const uo = this.data.dataToEdit;
    this.filterForm.patchValue({

      naturalezaOrgano: uo.tipoOrganoId,
      descripcion: uo.descripcion,
      sigla: uo.sigla,
      padreOrganigramaId: uo.unidadOrganicaSuperiorId,
    });
  } */

  saveOrgano() {
    this.filterForm.markAllAsTouched();
    const body = this.filterForm.getRawValue();
     if (this.filterForm.valid) {
      this.organigramaRepository.registerOrUpdateUO(body).subscribe(
         (res) => {
          if (res) {
            this.toastService.showToast(
              'Se realizó el registro exitosamente',
              'success'
            );
            this.onNoClick(true);
          } else {
            this.toastService.showToast(
              'Hubo un error al registrar la unidad orgánina',
              'danger'
            );
          }
        },
        (err) => this.toastService.showToast(err.message, 'danger')
      );
    }
  }

  editOrgano() {
    this.filterForm.markAllAsTouched();
    const body = this.filterForm.getRawValue();
    const idOrganigrama = this.data.dataToEdit.organigramaId;
    if (this.filterForm.valid) {
      this.organigramaRepository.registerOrUpdateUO(body, idOrganigrama).subscribe(
        (res) => {
          if (res) {
            this.toastService.showToast(
              'Se realizó la actualización exitosamente',
              'success'
            );
            this.onNoClick(true);
          } else {
            this.toastService.showToast(
              'Hubo un error al editar gestión de organigrama',
              'danger'
            );
          }
        },
        (err) => this.toastService.showToast(err.message, 'danger')
      );
    }
  }


  siglaValidator: AsyncValidatorFn = (control: AbstractControl): Observable<{ [key: string]: any } | null> => {
    return this.organigramaRepository.validarSiglaUnicidad(control.value).pipe(
      map(res => {
        if (res.codigo === 1 ) {
          this.mensaje = res.mensaje;
          return {uoError: {value: control.value}};
        }
        return null;
      })

    );
  }

  descripcionValidator: AsyncValidatorFn = (control: AbstractControl): Observable<{ [key: string]: any } | null> => {
      return this.organigramaRepository.validarDescripcionUnicidad(control.value).pipe(
        map(res => {
          if (res.codigo === 1 ) {
            this.mensaje = res.mensaje;
            return {uoError: {value: control.value}};
          }
          return null;
        })

      );
    }

}
export interface DataModel {
  createMode: boolean;
  dataToEdit: any;
  estados: any[];
  reprogramarMode?: boolean;
  lstHistoryReuniones: any[];
}

