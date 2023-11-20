import { Component, OnInit } from '@angular/core';
import { HelperLey1401Service } from '../helperLey1401.service';

@Component({
  selector: 'serv-talento-funciones',
  templateUrl: './funciones.component.html',
})
export class FuncionesComponent implements OnInit {
  constructor(public helper1401Service: HelperLey1401Service) {}

  ngOnInit(): void {}

  get f() {
    return this.helper1401Service.funcionesForm.controls;
  }
}
