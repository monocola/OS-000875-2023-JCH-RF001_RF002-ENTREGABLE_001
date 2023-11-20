import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
  selector: 'serv-talento-add-item-button',
  templateUrl: './add-item-button.component.html',
  styleUrls: ['./add-item-button.component.scss']
})
export class AddItemButtonComponent {

  @Output() clickEmitter = new EventEmitter();
  @Input() text = '';
  @Input() disable = false;

  addFunction() {
    this.clickEmitter.emit();
  }
}
