import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'serv-talento-tooltip-info',
  templateUrl: './tooltip-info.component.html',
  styleUrls: ['./tooltip-info.component.scss']
})
export class TooltipInfoComponent implements OnInit {

  @Input() message: string = '';

  constructor() { }

  ngOnInit(): void {
  }

}
