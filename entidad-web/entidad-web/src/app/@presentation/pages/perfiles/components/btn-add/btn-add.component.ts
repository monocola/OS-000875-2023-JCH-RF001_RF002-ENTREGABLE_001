import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
  selector: 'serv-talento-btn-add',
  templateUrl: './btn-add.component.html',
  styleUrls: ['./btn-add.component.scss'],
})
export class BtnAddComponent {
  @Output() clickEmitter = new EventEmitter();
  @Input() text = '';
  @Input() disableButton = false;

  addFunction() {
    this.clickEmitter.emit();
  }
}
