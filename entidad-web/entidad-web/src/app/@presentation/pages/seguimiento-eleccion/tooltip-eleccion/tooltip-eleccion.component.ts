import { Component, OnInit, Input } from '@angular/core';
import { FormGroup } from '@angular/forms';


@Component({
  selector: 'serv-talento-tooltip-eleccion',
  templateUrl: './tooltip-eleccion.component.html',
  styleUrls: ['./tooltip-eleccion.component.scss']
})
export class TooltipEleccionComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  postulantes: any;
  apellidos: any;
  nombres: any;
  @Input() porcentaje: number = 50;

  constructor(

  ) { }

  async ngOnInit() {
    this.title = 'Desestimar el contrato';
  }

}
