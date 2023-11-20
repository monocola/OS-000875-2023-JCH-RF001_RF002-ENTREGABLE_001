
import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Const } from 'src/app/@data/services/const';


@Component({
  selector: 'serv-talento-modal-convenio-entidad',
  templateUrl: './modal-convenio-entidad.component.html',
  styleUrls: ['./modal-convenio-entidad.component.scss']
})
export class ModalConvenioEntidadComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  TipoDoc = [
    { value: 1, documento: 'DNI' },
    { value: 4, documento: 'C.E' },

  ];
  EstadoConv = [];
  puestoRepreUni: any = '';
  nombRepreUni: any = '';
  nroDocRepreUni: any = '';
  direccionCentroEstudios: any = '';
  rucCentroEstudios: any = '';
  codigoTipoContrato: any = '';

  const = Const;


  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalConvenioEntidadComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toastService: ToastService

  ) { }

  async ngOnInit() {
    if (this.data.codigoTipoContrato === 'PRE1401') {
      this.dialogRef.updateSize('43%', '91%');

    } else {
      this.dialogRef.updateSize('43%', '60%');
    }
    this.title = 'Editar Datos de Entidad';
    this.text = this.data.text;
    this.puestoRepreUni = this.data.puestoRepreUni;
    this.nombRepreUni = this.data.nombRepreUni;
    this.nroDocRepreUni = this.data.nroDocRepreUni;
    this.direccionCentroEstudios = this.data.direccionCentroEstudios;
    this.rucCentroEstudios = this.data.rucCentroEstudios;
    this.codigoTipoContrato = this.data.codigoTipoContrato;

    this.initializeForm();
    this.fileName = 'Subir contenido';
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      puestoRepreUni: this.puestoRepreUni,
      nombRepreUni: this.nombRepreUni,
      TipoDoc: this.TipoDoc[0].value,
      nroDocRepreUni: this.nroDocRepreUni,
      direccionCentroEstudios: this.direccionCentroEstudios,
      rucCentroEstudios: this.rucCentroEstudios,

    });
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      const resolucion = {
        puestoRepreUni: this.f.puestoRepreUni.value,
        nombRepreUni: this.f.nombRepreUni.value,
        TipoDoc: this.f.TipoDoc.value,
        nroDocRepreUni: this.f.nroDocRepreUni.value,
        direccionCentroEstudios: this.f.direccionCentroEstudios.value,
        rucCentroEstudios: this.f.rucCentroEstudios.value,

      };

      this.onNoClick(resolucion);
    }
  }

  onNoClick(newData: any = false) {
    this.dialogRef.close(newData);
  }
}











