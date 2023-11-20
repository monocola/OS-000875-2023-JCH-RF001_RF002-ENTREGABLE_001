import { Component, OnInit } from '@angular/core';
import { AbstractControl } from '@angular/forms';
import { HelperLey30057Service } from '../helperLey30057.service';

@Component({
  selector: 'serv-talento-identificacion',
  templateUrl: './identificacion.component.html',
  styleUrls: ['./identificacion.component.scss'],
})
export class IdentificacionComponent implements OnInit {
  constructor(public helper30057Service: HelperLey30057Service) {}

  ngOnInit(): void {
    this.helper30057Service.loadCombox();
  }

  get f() {
    return this.helper30057Service.identificacionForm.controls;
  }

  validateCodPosicion(control: AbstractControl) {
    const value = Number(control.value);
    if (value > 100) {
      control.patchValue('100');
    } else if (control.value?.length > 0 && value < 1) {
      control.patchValue('1');
    }
  }

  validateCodPosicionACargo(control: AbstractControl) {
    const value = Number(control.value);
    if (value > 100) {
      control.patchValue('100');
    } else if (control.value.length > 0) {
      control.patchValue(value);
    }
  }

  changeGrupoServidores() {
    this.f.familiaPuestos.patchValue('');
    this.f.rol.patchValue('');
    this.helper30057Service.familiaPuestos = [];
    this.helper30057Service.roles = [];
    this.helper30057Service.getFamiliaPuestos(
      this.f.grupoServidoresCiviles.value.id
    );
    this.f.codigoPuesto.enable();
  }

  changeFamiliaPuestos() {
    this.helper30057Service.roles = [];
    this.f.rol.patchValue('');
    this.helper30057Service.getRoles(this.f.familiaPuestos.value.id);
    this.f.codigoPuesto.enable();
  }
}
