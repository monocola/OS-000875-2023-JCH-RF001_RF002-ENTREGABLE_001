import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { forkJoin } from 'rxjs';
import { EvaluacionesEntidadRepository } from 'src/app/@domain/repository/evaluaciones-entidad.repository';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { sortDataTableComponent } from 'src/app/utils/general';

@Component({
  selector: 'serv-talento-modal-jerarquia',
  templateUrl: './modal-jerarquia.component.html',
})
export class ModalJerarquiaComponent implements OnInit {
  regimenes = [];
  modalidades = [];
  tipos = [];
  tiposToShow = []; // Nuevo arreglo para filtrar tipos ya seleccionados

  jerarquias = [];
  jerarquiasColumn: TableColumn[] = [];

  jerarquiaForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private maestraEntidadService: MaestraEntidadRepository,
    private evaluacionesEntidadService: EvaluacionesEntidadRepository,
    private dialogRef: MatDialogRef<ModalJerarquiaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeColumns();
    this.loadCombox();
    this.initializeForm();
    if (this.data) {
      this.jerarquias = [...this.jerarquias, ...this.data.jerarquiasExistentes];
    }
  }

  initializeForm() {
    this.jerarquiaForm = this.fb.group({
      regimen: [''],
      modalidad: [{ value: '', disabled: true }],
      tipo: [{ value: '', disabled: true }],
    });
  }

  get f() {
    return this.jerarquiaForm.controls;
  }

  loadCombox() {
    const getRegimenesEntidad = this.maestraEntidadService.getCamposAsignadosByCodCabecera(
      'TBL_REGIMEN'
    );
    const getModalidadesEntidad = this.maestraEntidadService.getCamposAsignadosByCodCabecera(
      'TBL_MODALIDAD'
    );
    const getTiposEntidad = this.maestraEntidadService.getCamposAsignadosByCodCabecera(
      'TBL_TIPO'
    );
    forkJoin([
      getRegimenesEntidad,
      getModalidadesEntidad,
      getTiposEntidad,
    ]).subscribe((results) => {
      this.regimenes = results[0];
      this.modalidades = results[1];
      this.tipos = results[2];
    });
  }

  changeRegimen() {
    if (this.f.regimen.value) {
      this.f.modalidad.patchValue('');
      this.f.modalidad.enable();
      this.f.modalidad.updateValueAndValidity();
      this.f.tipo.patchValue('');
    } else {
      this.f.modalidad.patchValue('');
      this.f.modalidad.disable();
      this.f.modalidad.updateValueAndValidity();
      this.changeModalidad();
    }
  }

  changeModalidad() {
    if (this.f.modalidad.value) {
      this.f.tipo.patchValue('');
      this.f.tipo.enable();
      this.f.tipo.updateValueAndValidity();
      const form = this.jerarquiaForm.getRawValue();
      const regimenesSelected = this.jerarquias.filter(
        (j) => j.regimenId === form.regimen.maeDetalleId
      );
      const modalidadesSelected = regimenesSelected.filter(
        (j) => j.modalidadId === form.modalidad.maeDetalleId
      );
      const tiposSelected = modalidadesSelected.map((m) => m.tipoId);
      this.tiposToShow = this.tipos.filter(
        (t) => !tiposSelected.includes(t.maeDetalleId)
      );
    } else {
      this.f.tipo.patchValue('');
      this.f.tipo.disable();
      this.f.tipo.updateValueAndValidity();
    }
  }

  addJerarquia() {
    if (this.jerarquiaForm.invalid) return;
    const form = this.jerarquiaForm.getRawValue();
    this.jerarquias = [
      {
        existe: false,
        orden: this.jerarquias.length + 1,
        regimen: form.regimen.descripcion,
        regimenId: form.regimen.maeDetalleId,
        modalidad: form.modalidad.descripcion,
        modalidadId: form.modalidad.maeDetalleId,
        tipo: form.tipo.descripcion,
        tipoId: form.tipo.maeDetalleId,
        settings: {
          disableDelete: false,
        },
      },
      ...this.jerarquias,
    ];
    this.initializeForm();
  }

  generarJerarquias() {
    const newJerarquias = this.jerarquias.filter((j) => j.existe === false);
    if (newJerarquias.length > 0) {
      this.evaluacionesEntidadService
        .asignarEvaluacionesEntidad(newJerarquias)
        .subscribe(
          (res) => {
            if (res) {
              this.toastService.showToast(
                'Las jerarquías han sido registradas exitosamente',
                'success'
              );
              this.onNoClick(true);
            }
          },
          (err) => {
            this.toastService.showToast(err, 'danger');
          }
        );
    } else {
      this.toastService.showToast(
        'Tiene que seleccionar al menos una jerarquía',
        'primary'
      );
    }
  }

  onNoClick(newData = false) {
    this.dialogRef.close(newData);
  }

  // Tabla

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.jerarquias);
  }

  removeJerarquia(jerarquia) {
    this.changeModalidad();
    this.jerarquias = this.jerarquias.filter(
      (j) => j.orden !== jerarquia.orden
    );
  }

  initializeColumns() {
    this.jerarquiasColumn = [
      {
        name: '#',
        dataKey: 'orden',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'Régimen',
        dataKey: 'regimen',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'Modalidad',
        dataKey: 'modalidad',
        position: 'left',
        isSortable: true,
        width: '40%',
      },
      {
        name: 'Tipo',
        dataKey: 'tipo',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
    ];
  }
}
