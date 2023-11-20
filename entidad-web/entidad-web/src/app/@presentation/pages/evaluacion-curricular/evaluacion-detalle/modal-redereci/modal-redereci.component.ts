import { Component, Inject, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { armarPayload } from 'src/app/utils/utils';

@Component({
  selector: 'serv-talento-modal-redereci',
  templateUrl: './modal-redereci.component.html',

})
export class ModalRedereciComponent implements OnInit {
  form: FormGroup;
  title: string = '';
  postulantes: any;

  nombreApellido: string;

  constructor(
    private ref: MatDialogRef<ModalRedereciComponent>,
    private evaluacionCurricularService: EvaluacionCurricularRepository,
    private fb: FormBuilder,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {
    this.form = this.fb.group({
      comentario: new FormControl(null, Validators.required),
    });
  }

  ngOnInit(): void {
    this.ref.updateSize('37%', '58%');
    this.title = 'REDERECI';
    this.nombreApellido = this.data.nombreApellido;
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  get f() {
    return this.form.controls;
  }

  guardar() {
    let redereci = this.data.redereci ? '1' : '2';
    let item: any = {
      estadoRedereci: redereci,
      redereci_desc: this.f.comentario.value,
    };
    let request = armarPayload<any>(item);
    this.evaluacionCurricularService
      .actualizarRedereci(this.data.convocatoriaPostulante, request)
      .subscribe((res: any) => {
        this.dismiss(true);
      });
  }
}
