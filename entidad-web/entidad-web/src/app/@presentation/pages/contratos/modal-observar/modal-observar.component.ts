import {
  Component,
  Inject,
  OnInit,
} from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-modal-observar',
  templateUrl: './modal-observar.component.html',
  styleUrls: ['./modal-observar.component.scss'],
})
export class ModalObservarComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  EstadoConv = [];

  nroResolucion: any = '';
  nroInforme: string = '';
  periodoPrueba: string = '';
  codigoTipoContrato: any = '';
  nroMerito: any = '';



  const = Const;


  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalObservarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toastService: ToastService



  ) { }

  async ngOnInit() {
    console.info(this.data);
    this.title = 'Editar Datos de resolución y contrato';
    this.text = this.data.text;
    this.nroResolucion = this.data.nroResolucion;
    this.nroInforme = this.data.nroInforme;
    this.periodoPrueba = this.data.periodoPrueba;
    this.codigoTipoContrato = this.data.codigoTipoContrato;
    this.nroMerito = this.data.nroMerito;
    this.initializeForm();
    this.fileName = 'Subir contenido';
    if (this.data.codigoTipoContrato === 'SCC30057') {
      this.dialogRef.updateSize('33%', '55%');
    } else {
      this.dialogRef.updateSize('33%', '73%');
    }

  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      Nresolucion: this.nroResolucion,
      fechaResolucion: '',
      informe: [this.nroInforme, Validators.required],
      fechaVinculacion: ['', Validators.required],
      periodoPrueba: this.periodoPrueba,
      nroMerito: this.nroMerito,
    });
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      const resolucion = {
        Nresolucion: this.f.Nresolucion.value,
        fechaResolucion: this.f.fechaResolucion.value,
        informe: this.f.informe.value,
        fechaVinculacion: this.f.fechaVinculacion.value,
        periodoPrueba: this.f.periodoPrueba.value,
        nroMerito: this.f.nroMerito.value
      };

      this.onNoClick(resolucion);
    }
  }

  onNoClick(newData: any = false) {
    this.dialogRef.close(newData);
  }

}
