import { Component, OnInit } from '@angular/core';
import { HelperLey30057Service } from '../helperLey30057.service';

@Component({
  selector: 'serv-talento-funciones',
  templateUrl: './funciones.component.html',
  styleUrls: ['./funciones.component.scss'],
})
export class FuncionesComponent implements OnInit {
  constructor(public helper30057Service: HelperLey30057Service) {}

  ngOnInit(): void {}

  get f() {
    return this.helper30057Service.funcionesForm.controls;
  }

}
