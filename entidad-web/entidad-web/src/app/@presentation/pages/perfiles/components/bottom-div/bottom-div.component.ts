import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'serv-talento-bottom-div',
  templateUrl: './bottom-div.component.html',
  styleUrls: ['./bottom-div.component.scss'],
})
export class BottomDivComponent implements OnInit {
  @Input() indexStepper = 0;
  @Input() bodySize = '100vw';
  @Input() numberOfSteps = 4;

  @Output() previousStep = new EventEmitter();
  @Output() nextStep = new EventEmitter();
  @Output() createProfile = new EventEmitter();
  @Output() cancelClick = new EventEmitter();

  @Input() createMode = false;

  constructor() {}

  ngOnInit(): void {}
}
