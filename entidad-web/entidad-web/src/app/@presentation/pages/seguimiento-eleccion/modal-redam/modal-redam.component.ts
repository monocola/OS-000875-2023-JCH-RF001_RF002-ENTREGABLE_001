import { Component, Inject, OnInit } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SeguimientoRepository } from 'src/app/@domain/repository/seguimiento.repository';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';

@Component({
  selector: 'serv-talento-modal-redam',
  templateUrl: './modal-redam.component.html',
  styleUrls: ['./modal-redam.component.scss']
})
export class ModalRedamComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  postulantes: any;
  apellidos: any;
  nombres: any;
  descripcion: string = '';

  constructor(
    private dialogRef: MatDialogRef<ModalRedamComponent>,
    private seguimientoService: SeguimientoRepository,
    private seleccionService: SeleccionServirRepository,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  async ngOnInit() {
    this.dialogRef.updateSize('37%', '58%');
    this.title = 'Desestimar el contrato';

    this.apellidos = this.data.apellidos;
    this.nombres = this.data.nombres;
  }

  onClickGuardar() {
    this.onNoClick(true);
  }

  onNoClick(data = false) {
    this.dialogRef.close({ response: data, descripcion: this.descripcion });
  }
}


