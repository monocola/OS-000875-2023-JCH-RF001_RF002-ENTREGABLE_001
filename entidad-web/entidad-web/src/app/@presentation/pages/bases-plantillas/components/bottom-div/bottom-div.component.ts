import { Location } from '@angular/common';
import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'serv-talento-bottom-div',
  templateUrl: './bottom-div.component.html',
  styleUrls: ['./bottom-div.component.scss'],
})
export class BottomDivComponent implements OnInit {
  @Input() bodySize = '100vw';
  @Input() disabledSave = true;

  @Output() saveEmitter = new EventEmitter();

  constructor(private location: Location) {}

  ngOnInit(): void {}

  goBack() {
    this.location.back();
  }

  save() {
    this.saveEmitter.emit(null);
  }
}
