import { Component, Inject, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ModalConfirmationModel } from '../eliminar-evidencia/eliminar-evidencia.component';
import moment from 'moment';
import { NbDatepickerComponent } from '@nebular/theme';

@Component({
  selector: 'serv-talento-editar-evidencia',
  templateUrl: './editar-evidencia.component.html',
  styleUrls: ['./editar-evidencia.component.scss']
})
export class EditarEvidenciaComponent implements OnInit {

  @ViewChild(NbDatepickerComponent) formpicker;
  registerForm: FormGroup;
  private evidencia = null;

  constructor(
    private fb: FormBuilder,
    protected ref: MatDialogRef<EditarEvidenciaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ModalConfirmationModel
  ) {
    this.evidencia = data;
  }

  ngOnInit(): void {
    this.initializeForm();
    this.setInitData();
  }
  get f() {
    return this.registerForm.controls;
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      evidencia: ['', [Validators.required]],
      plazo: ['', [Validators.required]],
    });

    setTimeout(() => {
      const input = document.getElementById('plazo');


      input.setAttribute('maxlength', '10');


      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };


      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };
    }, 250);
  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
    console.log(keyCode);

    if (keyCode === 16) {
      this.isShift = true;
    }

    if (
      ((keyCode >= 48 && keyCode <= 57) ||
        keyCode === 8 ||
        keyCode === 46 ||
        keyCode === 37 ||
        keyCode === 39 ||
        (keyCode >= 96 && keyCode <= 105)) &&
      this.isShift === false
    ) {
      if (
        (input.value.length === 2 || input.value.length === 5) &&
        keyCode !== 8 && keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    } else {
      // Es invalido
    }
  }

  dismiss(success: boolean) {
    this.ref.close({
      result: success,
      data: this.evidencia,
    });
  }

  actualizar() {
    this.evidencia.evidencia = this.registerForm.controls.evidencia.value;
    this.evidencia.plazo = moment(
      this.registerForm.controls.plazo.value
    ).format('DD/MM/YYYY');
    this.ref.close({
      result: true,
      data: this.evidencia,
    });
  }

  private setInitData() {
    this.registerForm.controls.evidencia.setValue(this.evidencia.evidencia);
    this.registerForm.controls.plazo.setValue(
      moment(this.evidencia.plazo, 'DD/MM/YYYY').toDate()
    );
  }
}
