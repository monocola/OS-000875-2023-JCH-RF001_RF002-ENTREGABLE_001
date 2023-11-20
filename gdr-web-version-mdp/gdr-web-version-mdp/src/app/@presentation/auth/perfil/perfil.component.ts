import { Component, Inject } from '@angular/core';
import { FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

export interface DialogData {
  nombreRol: string;
  rolId: number;
}

@Component({
  selector: 'serv-talento-perfil',
  templateUrl: './perfil.component.html',
  styleUrls: ['./perfil.component.scss']
})
export class PerfilComponent {

  frm: FormGroup;
  lstRoles: any[] = [];
  checked = false;
  idEntidad: number;
  lstRol = null;
  img1 = './assets/images/rol1.png';
  img2 = './assets/images/rol2.png';
  img3 = './assets/images/rol3.png';

  constructor(
    private fb: FormBuilder,
    protected ref: MatDialogRef<PerfilComponent>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData
  ) {
    this.frm = this.fb.group({
      rolId: new FormControl(0, Validators.required),
      nombreRol: new FormControl(null, Validators.required),
  });
    this.lstRol = this.data;
    this.getIcono();
  }


  getIcono() {
    let val = 1;
    if (this.lstRol) {
      this.lstRol.forEach( item => {
        this.lstRoles.push({
          usuarioRolId: item.usuarioRolId,
          usuarioId: item.usuarioId,
          aplicacionId: item.aplicacionId,
          rolId: item.rolId,
          usuario: item.usuario,
          nombreAplicacion: item.nombreAplicacion,
          nombreRol: item.nombreRol,
          estadoRegistro: item.estadoRegistro,
          fechaInicioVigencia: item.fechaInicioVigencia,
          fechaFinVigencia: item.fechaFinVigencia,
          icono: (val === 1 ? this.img1 : val === 2 ? this.img2 : val === 3 ? this.img3 : this.img1),
          cuadro: false
        });
        val += 1;
      });
    }
  }

  get f() {
    return this.frm.controls;
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  getMenu() {
    let roles = this.lstRoles.find(item => item.rolId === this.frm.value.rolId);
    this.dismiss(roles);
  }

  seteaValor(perfil) {
    this.frm.controls['rolId'].setValue(Number(perfil.id));
    this.frm.controls['nombreRol'].setValue(perfil.name);
    this.checked = true;
    for (let i = 0; i < this.lstRoles.length; i++) {
      this.lstRoles[i].cuadro = this.lstRoles[i].rolId === Number(perfil.id);
    }
  }

}
