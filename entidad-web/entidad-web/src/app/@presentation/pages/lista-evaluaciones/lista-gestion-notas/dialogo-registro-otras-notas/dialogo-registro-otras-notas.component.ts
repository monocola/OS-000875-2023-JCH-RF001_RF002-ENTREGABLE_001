import { Component, Inject, OnInit, Optional } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { NbDialogRef } from '@nebular/theme';
import { RegistroOtrasNotasIndividualComponent } from './registro-otras-notas-individual/registro-otras-notas-individual.component';

@Component({
  selector: 'serv-talento-dialogo-registro-otras-notas',
  templateUrl: './dialogo-registro-otras-notas.component.html',
  styleUrls: ['./dialogo-registro-otras-notas.component.scss'],
})
export class DialogoRegistroOtrasNotasComponent implements OnInit {
  titulo: string;
  tipoEvaluacion: number;
  secondMessageTooltip: string;

  constructor(
    @Optional() protected ref: NbDialogRef<DialogoRegistroOtrasNotasComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    protected ref2: MatDialogRef<RegistroOtrasNotasIndividualComponent>,
  ) {
    this.titulo = this.data.tipo;
    this.tipoEvaluacion = this.data.tipoEvaluacion;
    this.secondMessageTooltip = this.titulo;
  }

  ngOnInit(): void {}

  changeTabRegistro(event): void {}

  dismiss(data = false) {
    this.ref2.close(data);
  }
}
