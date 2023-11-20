import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CabeceraMaestra } from 'src/app/@data/model/cabeceraMaestra';
import { CabeceraMaestraDetail } from 'src/app/@data/model/cabeceraMaestraDetail';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-creacion-detalle-maestra',
  templateUrl: './modal-creacion-detalle-maestra.component.html',
  styleUrls: ['./modal-creacion-detalle-maestra.component.scss'],
})
export class ModalCreacionDetalleMaestraComponent implements OnInit {
  constructor(
    private matDialog: MatDialogRef<ModalCreacionDetalleMaestraComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private maestraRepository: MaestraRepository,
    private toastService: ToastService,
    private fb: FormBuilder
  ) {}

  registerForm: FormGroup;

  ngOnInit(): void {
    this.initializeForm();
  }

  get f() {
    return this.registerForm.controls;
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      estado: ['0', [Validators.required]],
      tablaMaestra: ['', [Validators.required]],
      nombreCompleto: ['', [Validators.required]],
      nombreCorto: ['', [Validators.required]],
      sigla: ['', [Validators.required]],
      referencia: [''],
    });

    if (this.data.dataToEdit) {
      setTimeout(() => {
        this.updateForm();
      }, 0);
    }

    if (this.data.tablaMaestraSelected) {
      this.registerForm.patchValue({
        tablaMaestra: this.data.tablaMaestraSelected,
      });
    }
  }

  updateForm() {
    const tblMaestra = this.data.dataToEdit;
    this.registerForm.patchValue({
      estado: tblMaestra.estadoRegistro,
      tablaMaestra: tblMaestra.maeCabeceraId,
      nombreCompleto: tblMaestra.descripcion,
      nombreCorto: tblMaestra.descripcionCorta,
      sigla: tblMaestra.sigla,
      referencia: tblMaestra.referencia,
    });
    this.registerForm.get('tablaMaestra').disable();
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  saveMaestra() {
    this.registerForm.markAllAsTouched();
    const body = this.registerForm.getRawValue();
    if (this.registerForm.valid) {
      this.maestraRepository.createOrUpdateMaestraDetalle(body).subscribe(
        (res) => {
          if (res) {
            this.toastService.showToast(
              'La maestra se ha creado correctamente',
              'success'
            );
            this.onNoClick(this.registerForm.getRawValue());
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

  editMaestra() {
    this.registerForm.markAllAsTouched();
    const body = this.registerForm.getRawValue();
    const maeDetalleId = this.data.dataToEdit.maeDetalleId;
    if (this.registerForm.valid) {
      this.maestraRepository
        .createOrUpdateMaestraDetalle(body, maeDetalleId)
        .subscribe(
          (res) => {
            if (res) {
              this.toastService.showToast(
                'La maestra se ha editado correctamente',
                'success'
              );
              this.onNoClick(true);
            } else {
              this.toastService.showToast(
                'Hubo un error al editar la maestra',
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
  createMode: boolean;
  dataToEdit: CabeceraMaestraDetail;
  estados: any[];
  tablaMaestra: CabeceraMaestra[];
  tablaMaestraSelected?: number;
}
