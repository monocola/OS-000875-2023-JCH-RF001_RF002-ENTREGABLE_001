import { Location } from '@angular/common';
import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'serv-talento-bottom-div-entidad',
  templateUrl: './bottom-div-entidad.component.html',
  styleUrls: ['./bottom-div-entidad.component.scss'],
})
export class BottomDivEntidadComponent implements OnInit {
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
