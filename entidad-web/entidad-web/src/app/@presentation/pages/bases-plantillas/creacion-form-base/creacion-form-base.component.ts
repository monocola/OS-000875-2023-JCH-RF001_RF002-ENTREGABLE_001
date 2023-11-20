import { Component, OnInit } from '@angular/core';
import { CreacionFormBaseService } from './creacion-form-base.service';

@Component({
  selector: 'serv-talento-creacion-form-base',
  template: '<router-outlet></router-outlet>',
})
export class CreacionFormBaseComponent implements OnInit {
  constructor(private helperService: CreacionFormBaseService) {}

  ngOnInit(): void {
    this.helperService.loadData();
  }
}
