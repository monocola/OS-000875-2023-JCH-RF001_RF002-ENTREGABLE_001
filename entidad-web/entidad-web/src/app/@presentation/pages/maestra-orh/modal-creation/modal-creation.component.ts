import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MaestraEntidadService } from 'src/app/@data/services/maestra-entidad.service';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-creation',
  templateUrl: './modal-creation.component.html',
  styleUrls: ['./modal-creation.component.scss'],
})
export class ModalCreationComponent implements OnInit {
  registerForm: FormGroup;
  estados = [];

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalCreationComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private maestraEntidadService: MaestraEntidadService,
    private parametrosRepository: ParameterRepository,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    if (this.data.dataToEdit) {
      this.updateForm();
    }
    setTimeout(() => {
      this.getEstados();
    }, 0);
  }

  getEstados() {
    this.parametrosRepository
      .getEstadoRegistro()
      .subscribe((res) => (this.estados = res));
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      estado: ['1', Validators.required],
      nombre: ['', [Validators.required]],
      nombreCorto: ['', [Validators.required]],
      sigla: ['', [Validators.required]],
      referencia: [''],
    });
  }

  updateForm() {
    const values = this.data.dataToEdit;
    this.registerForm.patchValue({
      estado: values.estadoConfiguracion,
      nombre: values.descripcion,
      nombreCorto: values.descripcionCorta,
      sigla: values.sigla,
      referencia: values.referencia,
    });
  }

  onNoClick(flag: boolean = false) {
    this.dialogRef.close(flag);
  }

  get f() {
    return this.registerForm.controls;
  }

  registerOrEditMaestra() {
    const values = this.registerForm.getRawValue();
    const dataToEdit = this.data.dataToEdit;
    const cabecera = this.data.tablaMaestra;
    this.registerForm.markAllAsTouched();

    if (this.registerForm.valid) {
      this.maestraEntidadService
        .createOrUpdateMaestraDetalle(
          values,
          dataToEdit?.maeCabeceraId || cabecera.maeCabeceraId,
          dataToEdit?.configuracionId || null
        )
        .subscribe(
          (res) => {
            if (res) {
              this.toastService.showToast(
                'La maestra se ha creado correctamente',
                'success'
              );
              this.onNoClick(true);
            } else {
              this.toastService.showToast(
                'Hubo un error al registrar la maestra',
                'danger'
              );
            }
          },
          (err) => this.toastService.showToast(err.message, 'danger')
        );
    }
  }
}

export interface DataModel {
  dataToEdit: any;
  tablaMaestra: any;
}
