import { Component, Inject, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ConfirmarBonificacion } from 'src/app/@data/model/bonificaciones/bonificacion';
import { CboNivel } from 'src/app/@data/model/evaluacion-curricular/evaluacion';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';

@Component({
  selector: 'serv-talento-modal-bonificacion-confirm',
  templateUrl: './modal-bonificacion-confirm.component.html',
  styleUrls: ['./modal-bonificacion-confirm.component.scss'],
})
export class ModalBonificacionConfirmComponent implements OnInit {
  tipoBonificacion: number;
  form: FormGroup;
  lstNivel: CboNivel[];

  lstGeneric: any[] = [
    {
      bonificacionDetConvId: 1,
      descripcion: 'Aplica',
    },
    {
      bonificacionDetConvId: 2,
      descripcion: 'NoAplica',
    },
  ];

  constructor(
    private fb: FormBuilder,
    private matDialogRef: MatDialogRef<ModalBonificacionConfirmComponent>,
    private evaluacionCurricularService: EvaluacionCurricularRepository,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {
    this.form = this.fb.group({
      bonificacionPostulanteId: new FormControl(null),
      tipo: new FormControl(null, Validators.required),
      comentario: new FormControl('', [
        Validators.required,
        Validators.maxLength(200),
      ]),
    });
  }

  ngOnInit(): void {
    this.tipoBonificacion = this.data.tipoBonificacion;
    this.listarNivelDeportista(this.data.tipoBonificacion);

    if (this.data.patch) {
      this.form.patchValue(this.data.patch);
    }
  }

  // setearBonificacion(){
  //   this.evaluacionCurricularService
  //   .setearBonificacion(this.data.resumenId, this.data.tipoBonificacion)
  //   .subscribe((res: any) => {
  //     this.form.patchValue(res);
  //   });
  // }

  listarNivelDeportista(tipoBonificacion: number) {
    if (tipoBonificacion === 1) {
      this.evaluacionCurricularService
        .getNivelBonificacion(this.data.convocatoriaPostulante)
        .subscribe((res: CboNivel[]) => {
          this.lstNivel = res;
        });
    } else if (tipoBonificacion === 2) {
      this.lstNivel = this.lstGeneric;
    } else if (tipoBonificacion === 3) {
      this.lstNivel = this.lstGeneric;
    }
  }

  guardarBonificacion() {
    if (this.form.valid) {
      let item = this.mapearBonificacion();

      this.evaluacionCurricularService
        .guardarBonificacion(this.data.resumenId, item)
        .subscribe((res: any) => {
          this.dismiss(true);
        });
    }
  }

  mapearBonificacion(): any {
    let bono = new ConfirmarBonificacion();
    let form = this.form.value;

    bono.bonificacionPostulanteId = form.bonificacionPostulanteId;
    bono.comentario = form.comentario;
    bono.tipoBonificacion = this.data.tipoBonificacion;
    bono.tipo = form.tipo === '-1' ? null : form.tipo;
    if (this.data.tipoBonificacion === 1) {
      bono.flagAplica = form.tipo !== -1 ? 1 : 0;
    } else {
      bono.flagAplica = form.tipo === 1 ? 1 : 0;
    }

    return bono;
  }

  dismiss(success: boolean): void {
    this.matDialogRef.close(success);
  }

  get f() {
    return this.form.value;
  }
}
