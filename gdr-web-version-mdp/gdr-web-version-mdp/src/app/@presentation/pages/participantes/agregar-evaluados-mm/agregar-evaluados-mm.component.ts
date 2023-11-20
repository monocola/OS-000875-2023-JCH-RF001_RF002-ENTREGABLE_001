import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ServidoresRepository } from '../../../../@domain/repository/servidores.repository';
import { IParticipanteEvaluador } from '../../../../@data/model/participante';
import { FormControl, FormGroup, Validators } from '@angular/forms';

export interface AgregarEvaluadosData {
  evaluador: IParticipanteEvaluador;
}

@Component({
  selector: 'serv-talento-agregar-evaluados-mm',
  templateUrl: './agregar-evaluados-mm.component.html',
  styleUrls: ['./agregar-evaluados-mm.component.scss'],
})
export class AgregarEvaluadosMmComponent implements OnInit {
  evaluados: IParticipanteEvaluador[];
  evaluadosSeleccionados: IParticipanteEvaluador[];
  form: FormGroup;

  constructor(
    private matDialog: MatDialogRef<AgregarEvaluadosMmComponent>,
    private servidoresRepository: ServidoresRepository,
    @Inject(MAT_DIALOG_DATA) public data: AgregarEvaluadosData
  ) {
    this.form = new FormGroup({
      evaluados: new FormControl(null, [Validators.required]),
    });
  }

  ngOnInit(): void {
    this.servidoresRepository
      .listarSinEvaluador(this.data.evaluador)
      .subscribe((res) => {
        this.evaluados = res
      });
  }

  cerrar() {
    this.matDialog.close();
  }

  guardar() {

    if (this.form.valid) {
      const mandoMedio = this.data.evaluador;
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
    this.servidoresRepository
      .listarSinEvaluador(mandoMedio)
      .subscribe((res) => {
        this.evaluados = res;
      });
  }
}
