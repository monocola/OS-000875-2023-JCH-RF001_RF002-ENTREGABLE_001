import { Component, Input, OnInit } from '@angular/core';
import { HelperLey276Service } from '../helperLey276.service';

@Component({
  selector: 'serv-talento-funciones',
  templateUrl: './funciones.component.html',
})
export class FuncionesComponent implements OnInit {
  @Input() minimoActividades = 3;

  constructor(public helperService: HelperLey276Service) {}

  ngOnInit(): void {}

  get f() {
    return this.helperService.funcionesForm.controls;
  }
}
