import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'serv-talento-tooltip-info-reclutamiento',
  templateUrl: './tooltip-info-reclutamiento.component.html',
  styleUrls: ['./tooltip-info-reclutamiento.component.scss'],
})
export class TooltipInfoReclutamientoComponent implements OnInit {
  @Input() fechaInicio: string = '';
  @Input() fechaFin: string = '';
  @Input() porcentaje: number = 50;

  constructor() {}

  ngOnInit(): void {}
}
