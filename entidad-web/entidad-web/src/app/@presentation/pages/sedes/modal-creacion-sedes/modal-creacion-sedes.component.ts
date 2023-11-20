import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Sede } from 'src/app/@data/model/sede';
import { SedesRepository } from 'src/app/@domain/repository/sede.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-creacion-sedes',
  templateUrl: './modal-creacion-sedes.component.html',
  styleUrls: ['./modal-creacion-sedes.component.scss'],
})
export class ModalCreacionSedesComponent implements OnInit {
  registerForm: FormGroup;
  editionMode: boolean = false;
  arrayUbigeo: number[] = [];

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<ModalCreacionSedesComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private sedesRepository: SedesRepository,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeForm();
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  get f() {
    return this.registerForm.controls;
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      estado: ['0', [Validators.required]],
      nombreSede: ['', [Validators.required]],
      direccion: ['', [Validators.required]],
      departamento: ['', [Validators.required]],
      provincia: ['', [Validators.required]],
      distrito: ['', [Validators.required]],
      representante: [''],
      telefono: [''],
      anexo: [''],
    });

    if (this.data.dataToEdit) {
      setTimeout(() => {
        this.updateForm();
      }, 0);
    }
  }

  updateForm() {
    const sede = this.data.dataToEdit;
    this.arrayUbigeo = [sede.departamentoId, sede.provinciaId, sede.distritoId];
    this.registerForm.patchValue({
      estado: sede.estadoId,
      nombreSede: sede.nombreSede,
      direccion: sede.direccion,
      departamento: sede.departamentoId,
      provincia: sede.provinciaId,
      distrito: sede.distritoId,
      representante: sede.representante,
      telefono: sede.telefono,
      anexo: sede.anexo,
    });
  }

  saveOrgano() {
    this.registerForm.markAllAsTouched();
    const body = this.registerForm.getRawValue();
    if (this.registerForm.valid) {
      this.sedesRepository.registerOrUpdateSede(body).subscribe(
        (res) => {
          if (res) {
            this.toastService.showToast(
              'La sede se ha creado correctamente',
              'success'
            );
            this.onNoClick(true);
          } else {
            this.toastService.showToast(
              'Hubo un error al registrar la sede',
              'danger'
            );
          }
        },
        (err) => this.toastService.showToast(err.message, 'danger')
      );
    }
  }

  editOrgano() {
    this.registerForm.markAllAsTouched();
    const body = this.registerForm.getRawValue();
    const idSede = this.data.dataToEdit.sedeId;
    if (this.registerForm.valid) {
      this.sedesRepository.registerOrUpdateSede(body, idSede).subscribe(
        (res) => {
          if (res) {
            this.toastService.showToast(
              'La sede se ha editado correctamente',
              'success'
            );
            this.onNoClick(true);
          } else {
            this.toastService.showToast(
              'Hubo un error al editar la sede',
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
  dataToEdit: Sede;
  estados: any[];
}
