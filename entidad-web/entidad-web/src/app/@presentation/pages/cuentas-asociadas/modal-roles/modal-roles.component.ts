import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';
import { CuentaEntidadRepository } from 'src/app/@domain/repository/cuenta.entidad.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-roles',
  templateUrl: './modal-roles.component.html',
  styleUrls: ['./modal-roles.component.scss'],
})
export class ModalRolesComponent implements OnInit {
  funciones: any[] = [];
  roles: any[] = [];
  width = '';

  constructor(
    private dialogRef: MatDialogRef<ModalRolesComponent>,
    private cuentaEntidadServicio: CuentaEntidadRepository,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.getRoles();
  }

  onNoClick() {
    this.dialogRef.close();
  }

  getRoles() {
    this.cuentaEntidadServicio.getRolesForModal().subscribe(
      (res) => {
        this.roles = res.roles;
        this.funciones = res.body;
        this.width = (100 / (this.roles.length + 1)).toFixed(4) + '%';
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }
}
