import { Component, OnInit } from '@angular/core';
import { HelperLey1401Service } from '../helperLey1401.service';

@Component({
  selector: 'serv-talento-experiencia',
  templateUrl: './experiencia.component.html',
})
export class ExperienciaComponent implements OnInit {
  
  minExperiencias = 0;

  constructor(public helper1401Service: HelperLey1401Service) {}

  get f() {
    return this.helper1401Service.experienciaForm.controls;
  }

  ngOnInit(): void {}
}
