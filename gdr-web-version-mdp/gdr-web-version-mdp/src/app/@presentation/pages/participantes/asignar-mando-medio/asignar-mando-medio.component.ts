import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ServidoresRepository } from '../../../../@domain/repository/servidores.repository';
import { IParticipanteEvaluador } from '../../../../@data/model/participante';
import { FormControl, FormGroup, Validators } from '@angular/forms';

export interface AsignarMandoMedioData {
  evaluador: IParticipanteEvaluador;
}

@Component({
  selector: 'serv-talento-asignar-mando-medio',
  templateUrl: './asignar-mando-medio.component.html',
  styleUrls: ['./asignar-mando-medio.component.scss'],
})
export class AsignarMandoMedioComponent implements OnInit {
  evaluados: IParticipanteEvaluador[];
  evaluadosSeleccionados: IParticipanteEvaluador[];
  sinMandosMedio: IParticipanteEvaluador[];
  form: FormGroup;

  constructor(
    private matDialog: MatDialogRef<AsignarMandoMedioComponent>,
    private servidoresRepository: ServidoresRepository,
    @Inject(MAT_DIALOG_DATA) public data: AsignarMandoMedioData
  ) {
    this.form = new FormGroup({
      mandoMedio: new FormControl(null, [Validators.required]),
      evaluados: new FormControl(null, [Validators.required]),
    });
  }

  ngOnInit(): void {
    this.servidoresRepository
      .listarSinMandoMedio(this.data.evaluador)
      .subscribe((res) => {
        this.sinMandosMedio = res;
      });
  }

  cerrar() {
    this.matDialog.close();
  }

  guardar() {
    if (this.form.valid) {
      const mandoMedio = this.form.value.mandoMedio as IParticipanteEvaluador;
      const evaluados = this.form.value.evaluados as IParticipanteEvaluador[];
      this.servidoresRepository
        .agregarMandoMedio(mandoMedio, evaluados)
        .subscribe((r) => {
          this.matDialog.close(r);
        });
    } else {
      this.form.markAllAsTouched();
    }
  }

  onMandoMedioChange(mandoMedio: IParticipanteEvaluador) {
    this.evaluados = []
    this.form.patchValue({
      evaluados: []
    });
    this.servidoresRepository
      .listarSinEvaluador(mandoMedio)
      .subscribe((res) => {
        this.evaluados = res;
      });
  }
}
