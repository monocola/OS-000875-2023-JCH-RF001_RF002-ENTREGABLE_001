import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';

@Component({
  selector: 'serv-talento-modal-publicar',
  templateUrl: './modal-publicar.component.html',
  styleUrls: ['./modal-publicar.component.scss'],
})
export class ModalPublicarComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  comunicados = [];
  EstadoConv = [];
  valor: any = '';
  constructor(
    private maestraService: MaestraRepository,
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalPublicarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.dialogRef.updateSize('33%', '47%');
    this.initializeForm();
    this.fileName = 'Subir contenido';
    this.getTipoComunicados();
    this.getTipoEtapaConv();
  }

  get f() {
    return this.Form.controls;
  }

  getTipoComunicados() {
    this.maestraService.getMaestraDetalleByCod('TIP_COMU').subscribe((res) => {
      let desarrollo = res;
      this.comunicados = desarrollo.filter((item) => {
        return item.maeDetalleId === this.data.titulo;
      });
      this.title = this.comunicados[0].descripcion;
    });
  }

  getTipoEtapaConv() {
    this.maestraService
      .getMaestraDetalleByCod('TIP_EST_CONV')
      .subscribe((res) => {
        let estadoConvocatoria = res;
        let EstadoConv1 = estadoConvocatoria.filter((item) => {
          return item.codProg === '2';
        });
        let EstadoConv2 = estadoConvocatoria.filter((item) => {
          return item.codProg === '3';
        });
        this.EstadoConv = [...EstadoConv1, ...EstadoConv2];
      });
  }

  valorCambio() {}

  initializeForm() {
    this.Form = this.fb.group({
      EstadoConv: null,
    });
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      this.onNoClick(this.f.EstadoConv.value);
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
