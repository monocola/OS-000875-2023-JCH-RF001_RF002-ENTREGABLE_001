import { NbDialogRef } from '@nebular/theme';
import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'serv-talento-dialog-confirm-rnssc',
  templateUrl: './dialog-confirm-rnssc.component.html',
  styleUrls: ['./dialog-confirm-rnssc.component.scss']
})
export class DialogConfirmRnsscComponent implements OnInit {

  constructor(protected ref: NbDialogRef<DialogConfirmRnsscComponent>) { }

  ngOnInit(): void {
  }


  dismiss(success: boolean) {
    this.ref.close(success);
  }

  validarRnssc() {

  }
}
