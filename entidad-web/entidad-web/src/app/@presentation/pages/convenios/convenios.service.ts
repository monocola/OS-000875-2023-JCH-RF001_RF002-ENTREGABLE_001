import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';

@Injectable({
  providedIn: 'root'
})
export class ConveniosService {
  form: FormGroup;
  formConvenio: FormGroup;
  constructor(private fb: FormBuilder) { }

  initializeValues() {
    this.initializeForm();
  }


  enviarContrato(
    postulanteSelId: number,
    estado: string,
    idContrato: string,
    codProEstado: number,

  ) {
    this.formConvenio.patchValue({
      postulanteSelId: postulanteSelId,
      estado: estado,
      idContrato: idContrato,
      codProEstado: codProEstado,

    });
  }



  enviarBase(resp) {
    this.formConvenio.patchValue({
      postulanteSelId: resp.postulanteSelId,
      estado: resp.estado,
      idContrato: resp.idContrato,
      codProEstado: resp.idContrato,

    });
  }

  initializeForm() {
    this.form = this.fb.group({
      postulanteSelId: 0,
      estado: '',
      idContrato: 0,
      codProEstado: 0,


    });

    this.formConvenio = this.fb.group({
      postulanteSelId: 0,
      estado: '',
      idContrato: 0,
      codProEstado: 0,

    });
  }


}


