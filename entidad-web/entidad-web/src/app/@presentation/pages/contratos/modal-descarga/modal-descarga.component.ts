import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ContratosService } from '../contratos.service';
import { DesestimientoRepository } from 'src/app/@domain/repository/desestimiento.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import FileSaver from 'file-saver';
@Component({
  selector: 'serv-talento-modal-descarga',
  templateUrl: './modal-descarga.component.html',
  styleUrls: ['./modal-descarga.component.scss'],
})
export class ModalDescargaComponent implements OnInit {
  contrato: any = '';
  validador: number;

  constructor(
    private dialogRef: MatDialogRef<ModalDescargaComponent>,
    public helperService: ContratosService,
    private DesestimientoContratoService: DesestimientoRepository,
    private toast: ToastService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    this.contrato = this.data.contratoId;
    this.validador = this.data.validador;
  }

  get g() {
    return this.helperService.formContrato.controls;
  }

  descargarContratoW() {
    if (this.validador === 2) {
      this.DesestimientoContratoService.DescargarContrato(
        this.contrato
      ).subscribe((res) => {
        let base64String =
          'data:application/vnd.openxmlformats-officedocument.wordprocessingml.document;base64,' +
          res;
        FileSaver.saveAs(base64String, 'Contrato');
        this.onNoClick(true);
      });
    } else {
      this.onNoClick(true);
      this.toast.showToast('Descarga de documento con éxito ', 'success');
    }
  }

  onClickDescargar() {
    this.onNoClick(true);
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
