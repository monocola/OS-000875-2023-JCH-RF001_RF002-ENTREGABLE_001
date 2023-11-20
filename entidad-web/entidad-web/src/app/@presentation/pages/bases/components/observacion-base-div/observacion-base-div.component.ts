import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ObservacionStepBase } from '../../creacion-base/creacion-base.service';

@Component({
  selector: 'serv-talento-observacion-base-div',
  templateUrl: './observacion-base-div.component.html',
  styleUrls: ['./observacion-base-div.component.scss'],
})
export class ObservacionBaseDivComponent implements OnInit {
  @Input() showButtons = true;
  @Input() onlyShow = false;
  @Input() observacion: ObservacionStepBase = {
    description: '',
    resuelto: false,
    step: 0,
  };
  @Output() editEmitter = new EventEmitter();
  @Output() deleteEmitter = new EventEmitter();
  @Output() resolverEmitter = new EventEmitter();

  constructor(private dialog: MatDialog) {}

  ngOnInit(): void {}

  deleteObservacion() {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Remover observación',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.deleteEmitter.emit();
      }
    });
  }
}
