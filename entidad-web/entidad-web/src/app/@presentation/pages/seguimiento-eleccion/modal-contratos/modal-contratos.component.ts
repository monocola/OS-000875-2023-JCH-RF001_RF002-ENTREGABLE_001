import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-contratos',
  templateUrl: './modal-contratos.component.html',
  styleUrls: ['./modal-contratos.component.scss'],
})
export class ModalContratosComponent implements OnInit {
  Form: FormGroup;
  lista = [];
  vacantes: number = 0;
  finalistas: number = 0;
  TipoDoc: any = [];

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalContratosComponent>,
    private seguimientoService: SeguimientoRepository,
    private toast: ToastService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  ngOnInit(): void {
    this.dialogRef.updateSize('35%', '68%');

    this.data.listaAptos.forEach((element) => {
      let postulante = {
        baseId: element.baseId,
        postulanteId: element.postulanteSelId,
        perfilId: element.perfilId,
        entidadId: element.entidadId,
      };
      this.lista.push(postulante);
    });
    this.vacantes = this.data.vacantes;
    this.finalistas = this.data.finalistas;

    this.TipoDoc = this.data.dataTipoContrato.lstTipoContrato;
    this.initializeForm();
  }

  get f() {
    return this.Form.controls;
  }
  initializeForm() {
    this.Form = this.fb.group({
      TipoDoc: [this.TipoDoc.tipoContratoId, Validators.required],
    });
  }

  onClickGuardar() {
    if(this.lista.length > 0 ) {
      this.seguimientoService
        .CrearContratoOrConvenio(this.data.regimen, this.lista, this.f.TipoDoc.value)
        .subscribe(
          (res) => {
            this.onNoClick(true);
            this.toast.showToast(
              'Los finalistas se agregaron se agregaron con éxito a contratos/convenios.',
              'success',
              'Atención'
            );
          },
          (error) => {
            console.info(error);
            this.onNoClick(false);
            this.toast.showToast(
              'El servicio no esta disponible.',
              'danger',
              'Atención'
            );
          }
        );
    } else {
      this.toast.showToast( 'No cuenta con finalistas disponibles.', 'danger', 'Atención');
    }
  }
  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
