import { Component, Inject, OnInit, Optional } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { NbDialogRef } from '@nebular/theme';
import { RegistroOtrasNotasIndividualComponent } from '../dialogo-registro-otras-notas/registro-otras-notas-individual/registro-otras-notas-individual.component';
@Component({
  selector: 'serv-talento-dialog-registro-notas-eval',
  templateUrl: './dialog-registro-notas-eval.component.html',
  styleUrls: ['./dialog-registro-notas-eval.component.scss']
})
export class DialogRegistroNotasEvalComponent implements OnInit {

  titulo: string;
  tipoEvaluacion: number;
  secondMessageTooltip: string;

  constructor(
    @Optional() protected ref: NbDialogRef<DialogRegistroNotasEvalComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    protected ref2: MatDialogRef<RegistroOtrasNotasIndividualComponent>
  ) {
    this.titulo = this.data.tipo;
    this.secondMessageTooltip = this.data.tipo;
  }

  ngOnInit(): void {
  }

  changeTabRegistro(event): void {

  }

  dismiss(success: boolean) {
    this.ref2.close(success);
  }

  guardar() {

  }

}
