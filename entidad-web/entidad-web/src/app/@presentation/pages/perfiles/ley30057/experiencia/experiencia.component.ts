import { Component, Input, OnInit } from '@angular/core';
import { monthsValues, yearsValues } from '../../../../../utils/values';

@Component({
  selector: 'serv-talento-experiencia',
  templateUrl: './experiencia.component.html',
  styleUrls: ['./experiencia.component.scss'],
})
export class ExperienciaComponent implements OnInit {
  @Input() helper30057Service: any;

  funciones: any[] = [];
  requisitos: any[] = [];

  years = yearsValues;
  months = monthsValues;

  ngOnInit(): void {}

  get f() {
    return this.helper30057Service.experienciaForm.controls;
  }
}
