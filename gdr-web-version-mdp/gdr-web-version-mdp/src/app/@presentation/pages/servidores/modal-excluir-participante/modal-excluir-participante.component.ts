import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';

@Component({
  selector: 'serv-talento-modal-excluir-participante',
  templateUrl: './modal-excluir-participante.component.html',
  styleUrls: ['./modal-excluir-participante.component.scss']
})
export class ModalExcluirParticipanteComponent implements OnInit {
  tipoEstado;
  dataEstado: any[] = [];
  checked = false;

  constructor(
    protected ref: MatDialogRef<ModalExcluirParticipanteComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DataEstados,
    private toastService: ToastService,
  ) { }

  ngOnInit(): void {
    this.data.estados.forEach(item => {
      this.dataEstado.push({estados: item, seleccionar: false});
    });
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  opcion(events, i) {
    for (let j = 0; j < this.dataEstado.length; j++) {
      this.dataEstado[j].seleccionar = false;
      if ( i === j ) {
        this.dataEstado[j].seleccionar = true;
        this.checked = true;
      }
    }
    this.tipoEstado = Number(events);
  }

  aceptar() {
    if ( this.tipoEstado ) {
      this.dismiss(this.tipoEstado);
    } else {
      this.toastService.showToast('Seleccione una opción', 'danger');
    }

  }

}
export interface DataEstados {
  estados: any[];
  items: any[];
}
