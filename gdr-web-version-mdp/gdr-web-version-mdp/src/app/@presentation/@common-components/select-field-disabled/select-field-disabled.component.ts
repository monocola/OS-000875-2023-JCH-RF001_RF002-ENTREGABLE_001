import { Component, EventEmitter, Input, Output } from '@angular/core';
import { AbstractControl } from '@angular/forms';

@Component({
  selector: 'select-field-disabled',
  templateUrl: './select-field-disabled.component.html',
  styleUrls: ['./select-field-disabled.component.scss'],
})
export class SelectFieldDisabledComponent {
  @Input() label = 'DEFAULT__LABEL';
  @Input() labelOptionDefault = 'Elige';
  @Input() placeholder = 'Elige';
  @Input() items = [];
  @Input() control: AbstractControl;
  @Input() size = 'large';
  @Input() hideDefaultOption: boolean = false;
  @Input() fullWidth: boolean = true;
  @Input() multiple: boolean = false;

  @Input() value = null;
  @Input() valueToShow = null;

  @Output() selectedChange = new EventEmitter();
  @Output() blur = new EventEmitter();

  changeValue() {
    this.selectedChange.emit(this.control.value);
  }

  compareFn(c1: any, c2: any): boolean {
    return c1 && c2 ? c1.maeDetalleId === c2.maeDetalleId : c1 === c2;
  }
}
