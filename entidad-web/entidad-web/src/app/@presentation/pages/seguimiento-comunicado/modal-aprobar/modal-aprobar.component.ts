import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';

@Component({
  selector: 'serv-talento-modal-aprobar',
  templateUrl: './modal-aprobar.component.html',
  styleUrls: ['./modal-aprobar.component.scss'],
})
export class ModalAprobarComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  comunicados = [];
  constructor(
    private maestraService: MaestraRepository,
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalAprobarComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.dialogRef.updateSize('33%', '44%');
    this.initializeForm();
    this.fileName = 'Subir contenido';
    this.getTipoComunicados();
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      observadorText: '',
      comunicado: '',
    });
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

  GuardarComunicado() {
    if (this.Form.valid) {
      this.onNoClick(true);
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
