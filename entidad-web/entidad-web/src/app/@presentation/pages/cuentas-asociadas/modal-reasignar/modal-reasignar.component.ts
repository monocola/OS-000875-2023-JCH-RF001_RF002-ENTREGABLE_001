import { Component, OnInit } from '@angular/core';
import { NbDialogRef } from '@nebular/theme';

@Component({
  selector: 'serv-talento-modal-reasignar',
  templateUrl: './modal-reasignar.component.html',
  styleUrls: ['./modal-reasignar.component.scss'],
})
export class ModalReasignarComponent implements OnInit {
  loading: boolean = false;
  submitted = false;
  error = '';

  constructor(protected ref: NbDialogRef<ModalReasignarComponent>) {}

  ngOnInit() {}

  dismiss(success: boolean) {
    this.ref.close(success);
  }
}
