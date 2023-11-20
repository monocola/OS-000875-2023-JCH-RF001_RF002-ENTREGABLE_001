import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-activar-participante',
  templateUrl: './modal-activar-participante.component.html',
  styleUrls: ['./modal-activar-participante.component.scss']
})
export class ModalActivarParticipanteComponent implements OnInit {

  constructor(
    protected ref: MatDialogRef<ModalActivarParticipanteComponent>,
  ) { }

  ngOnInit(): void {
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }
}
