import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import moment from 'moment';

@Component({
  selector: 'serv-talento-nueva-evidencia',
  templateUrl: './nueva-evidencia.component.html',
  styleUrls: ['./nueva-evidencia.component.scss'],
})
export class NuevaEvidenciaComponent implements OnInit {
  registerForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<NuevaEvidenciaComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private servidoresRepository: ServidoresRepository,
    private toastService: ToastService,
    protected ref: MatDialogRef<NuevaEvidenciaComponent>
  ) {}

  listEvidencias: any[] = [];
  evidencia: any = null;

  ngOnInit(): void {
    this.initializeForm();
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
      const input = document.getElementById('plazoID');


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
    this.ref.close(success);
  }

  registrar() {
    this.registerForm.markAllAsTouched();
    if (this.registerForm.valid) {
      this.evidencia = null;
      const frm = this.registerForm.value;
      const body = {
        plazo: moment(frm.plazo).format('DD/MM/YYYY'),
        evidencia: frm.evidencia,
        estadoRegistro: '1',
      };
      this.evidencia = body;
      this.dismiss(this.evidencia);
      //  entidadId: this.profile.entidadId,
    }

    /*  this.servidoresRepository.agregarEvidencia(body)
        .subscribe(x => {
          this.cerrar();
        },
        (err) => this.toastService.showToast(err, 'danger')); */
  }
}
