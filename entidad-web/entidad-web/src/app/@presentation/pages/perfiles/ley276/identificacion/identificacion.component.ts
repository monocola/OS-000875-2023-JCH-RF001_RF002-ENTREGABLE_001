import { Component, OnInit } from '@angular/core';
import { AbstractControl } from '@angular/forms';
import { HelperLey276Service } from '../helperLey276.service';

@Component({
  selector: 'serv-talento-identificacion',
  templateUrl: './identificacion.component.html',
  styleUrls: ['./identificacion.component.scss'],
})
export class IdentificacionComponent implements OnInit {
  constructor(public helperService: HelperLey276Service) {}

  ngOnInit(): void {
    this.helperService.loadCombox();
  }

  get f() {
    return this.helperService.identificacionForm.controls;
  }

  validateCodPosicionACargo(control: AbstractControl) {
    const value = Number(control.value);
    if (value > 100) {
      control.patchValue('100');
    } else if (control.value.length > 0) {
      control.patchValue(value);
    }
  }
}
