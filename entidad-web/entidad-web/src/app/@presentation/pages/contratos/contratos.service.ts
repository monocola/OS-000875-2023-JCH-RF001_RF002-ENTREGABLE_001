import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';

@Injectable({
  providedIn: 'root'
})
export class ContratosService {
  form: FormGroup;
  formContrato: FormGroup;
  constructor(private fb: FormBuilder) { }

  initializeValues() {
    this.initializeForm();
  }


  enviarContrato(
    postulanteSelId: number,
    idEstado: string,
    idContrato: string,
    codProEstado: number,

  ) {
    this.formContrato.patchValue({
      postulanteSelId: postulanteSelId,
      estado: idEstado,
      idContrato: idContrato,
      codProEstado: codProEstado,

    });
  }

/*   descargarContratoW(
    idContrato: string
  ){

  } */



  enviarBase(resp) {
    this.formContrato.patchValue({
      postulanteSelId: resp.postulanteSelId,
      estado: resp.estado,
      idContrato: resp.idContrato,
      codProEstado: resp.codProEstado,
    });
  }

  initializeForm() {
    this.form = this.fb.group({
      postulanteSelId: 0,
      estado: '',
      idContrato: 0,
      codProEstado: 0,

    });

    this.formContrato = this.fb.group({
      postulanteSelId: 0,
      estado: '',
      idContrato: 0,
      codProEstado: 0,

    });
  }


}


