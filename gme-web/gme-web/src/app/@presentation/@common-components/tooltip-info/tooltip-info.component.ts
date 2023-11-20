import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'gme-web-tooltip-info',
  templateUrl: './tooltip-info.component.html',
  styleUrls: ['./tooltip-info.component.scss']
})
export class TooltipInfoComponent  {

  @Input() message: string = '';

}
